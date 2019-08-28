namespace NCoreUtils.Images.WebService

open System.IO
open System.Text
open System.Threading
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open NCoreUtils
open NCoreUtils.AspNetCore
open NCoreUtils.DependencyInjection
open NCoreUtils.Images
open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open System.Runtime.ExceptionServices
open System.Threading.Tasks
open NCoreUtils.IO

module ValueOption = Microsoft.FSharp.Core.ValueOption

type ServiceConfiguration () =
  member val MaxConcurrentOps = 20 with get, set
  override this.ToString () = sprintf "ServiceConfiguration[MaxConcurrentOps = %d]" this.MaxConcurrentOps

type ResizerServices = {
  Resizer              : IImageResizer
  SourceExtractor      : IImageSourceExtractor
  DestinationExtractor : IImageDestinationExtractor }

module internal Middleware =

  [<Sealed>]
  type Logger () = class end

  type private HttpResponseDestination (response : HttpResponse) =
    interface IImageDestination with
      member __.AsyncWrite (info, generator) =
        response.ContentType <-
          match info.ContentType with
          | null -> "application/octet-stream"
          | ct   -> ct
        match info.ContentLength with
        | Nullable.Value _ as contentLength ->
          response.ContentLength <- contentLength
          generator response.Body
        | _ -> async {
          use buffer = new FileStream (Path.GetTempFileName (), FileMode.Create, FileAccess.ReadWrite, FileShare.None, 65536, FileOptions.Asynchronous ||| FileOptions.DeleteOnClose)
          do! generator buffer
          buffer.Seek (0L, SeekOrigin.Begin) |> ignore
          response.ContentLength <- Nullable.mk buffer.Length
          do! buffer.AsyncCopyTo (response.Body, 65536)
          do! response.Body.AsyncFlush () }
      member this.AsyncWriteDelayed generator =
        let setContentType (info : ContentInfo) =
          response.ContentType <-
            match info.ContentType with
            | null -> "application/octet-stream"
            | ct   -> ct
        generator setContentType response.Body


  let private errorSerializerSettings =
    let settings = JsonSerializerSettings (ReferenceLoopHandling = ReferenceLoopHandling.Ignore, ContractResolver = CamelCasePropertyNamesContractResolver ())
    settings.Converters.Insert (0, ImageResizerErrorConverter ())
    settings

  let inline private (|CI|_|) (comparand : string) (value : CaseInsensitive) =
    match CaseInsensitive comparand = value with
    | true -> Some ()
    | _    -> None

  let private readParameters (query : IQueryCollection) =
    let inline v name =
      let mutable values = Unchecked.defaultof<_>
      match query.TryGetValue (name, &values) with
      | true when values.Count > 0 -> Some values.[0]
      | _                          -> None
    let inline s name = v name |> Option.toObj
    let inline i name = v name >>= tryInt |> Option.toNullable
    let inline b name = v name >>= tryBool |> Option.toNullable
    ResizeOptions (
      imageType  = s "t",
      width      = i "w",
      height     = i "h",
      resizeMode = s "m",
      quality    = i "q",
      optimize   = b "x",
      weightX    = i "cx",
      weightY    = i "cy")

  let private extractSource httpContext (sourceExtractor : IImageSourceExtractor) = async {
    match! sourceExtractor.AsyncExtractImageSource httpContext with
    | ValueSome extractor -> return! extractor.AsyncCreateProducer ()
    | ValueNone           -> return  StreamProducer.ofStreamWithCleanUp httpContext.Request.Body false }

  let private extractDestination httpContext (destinationExtractor : IImageDestinationExtractor) = async {
    match! destinationExtractor.AsyncExtractImageDestination httpContext with
    | ValueSome destination -> return destination
    | ValueNone             ->
      return! DefaultImageDestinationExtractor.SharedInstance.AsyncExtractImageDestination httpContext
      >>| ValueOption.get }

  let info httpContext { Resizer = imageResizer; SourceExtractor = sourceExtractor } =
    extractSource httpContext sourceExtractor
    >>= imageResizer.AsyncGetImageInfo
    >>= json httpContext

  let handle (computation : unit -> Async<_>) = async {
    try return! computation () >>| Ok
    with
      | :? ImageResizerException as exn -> return exn.Error |> Error
      | exn                             -> ExceptionDispatchInfo.Capture(exn).Throw (); return Unchecked.defaultof<_> }


  let private acquire (logger : ILogger) (target: bool ref) (semaphore : SemaphoreSlim) =
    let job (cancellationToken : CancellationToken) =
      // force setting value even if cancelled
      let acquireTask = semaphore.WaitAsync cancellationToken
      // on success
      acquireTask.ContinueWith(
        (fun (t : Task) ->
          logger.LogInformation ("[counter = {0}] Successfully acquired lock.", semaphore.CurrentCount)
          target := t.IsCompletedSuccessfully),
        CancellationToken.None,
        TaskContinuationOptions.OnlyOnRanToCompletion,
        TaskScheduler.Current
      ) |> ignore
      // on failure
      acquireTask.ContinueWith(
        (fun (t : Task) ->
          logger.LogInformation ("[counter = {0}] Failed to acquire lock.", semaphore.CurrentCount)
          target := false),
        CancellationToken.None,
        TaskContinuationOptions.OnlyOnFaulted,
        TaskScheduler.Current
      ) |> ignore
      // on cancelled
      acquireTask.ContinueWith(
        (fun (t : Task) ->
          logger.LogInformation ("[counter = {0}] Acquiring lock has been cancelled.", semaphore.CurrentCount)
          target := false),
        CancellationToken.None,
        TaskContinuationOptions.OnlyOnCanceled,
        TaskScheduler.Current
      ) |> ignore
      // pass original task
      acquireTask
    Async.Adapt job


  let private resizeRes (semaphore : SemaphoreSlim) httpContext { Resizer = imageResizer; SourceExtractor = sourceExtractor; DestinationExtractor = destinationExtractor } = async {
    let options = (HttpContext.request httpContext).Query |> readParameters
    let logger = getRequiredService<ILogger<Logger>> httpContext.RequestServices
    logger.LogInformation ("[counter = {0}] Starting resize...", semaphore.CurrentCount)
    let acquired = ref false
    let released = ref 0
    use _ =
      httpContext.RequestAborted.Register
        (fun () ->
          match !acquired with
          | true ->
            if 0 = Interlocked.CompareExchange (released, 1, 0) then
              let semCount = semaphore.Release()
              logger.LogInformation ("[counter = {0}] Request aborted.", semCount + 1)
          | false ->
            logger.LogInformation ("[counter = {0}] Request aborted proior acquiring.", semaphore.CurrentCount)
        )
    try
      do! acquire logger acquired semaphore
      let! source      = extractSource      httpContext sourceExtractor
      let! destination = extractDestination httpContext destinationExtractor
      return! handle (fun () -> imageResizer.AsyncResize (source, destination, options))
    finally
      do
        match !acquired && 0 = Interlocked.CompareExchange (released, 1, 0) with
        | true ->
          let semCount = semaphore.Release ()
          logger.LogInformation ("[counter = {0}] Finished resizing.", semCount + 1)
        | _ ->
          logger.LogInformation ("[counter = {0}] No release needed.", semaphore.CurrentCount) }

  let private resize semaphore httpContext imageResizer = async {
    let! res = resizeRes semaphore httpContext imageResizer
    match res with
    | Ok _ -> ()
    | Error err ->
      let response = HttpContext.response httpContext
      response.StatusCode <- 400
      response.ContentType <- "application/json"
      let json = JsonConvert.SerializeObject (err, errorSerializerSettings)
      let data = Encoding.UTF8.GetBytes json
      response.ContentLength <- Nullable.mk data.LongLength
      do! response.Body.AsyncWrite data
      do! response.Body.AsyncFlush () }

  let inline private async405 httpContext =
    HttpContext.setResponseStatusCode 405 httpContext
    async.Return ()

  let run semaphore httpContext asyncNext =
    match HttpContext.path httpContext with
    | [] ->
      match HttpContext.httpMethod httpContext with
      | HttpMethod.HttpPost -> bindServices<ResizerServices> httpContext.RequestServices |> resize semaphore httpContext
      | _                   -> async405 httpContext
    | [ CI "info" ] ->
      match HttpContext.httpMethod httpContext with
      | HttpMethod.HttpPost -> bindServices<ResizerServices> httpContext.RequestServices |> info httpContext
      | _                   -> async405 httpContext
    | [ CI "capabilities" ] ->
      match HttpContext.httpMethod httpContext with
      | HttpMethod.HttpGet  -> json httpContext [| Capabilities.serializedImageInfo |]
      | _                   -> async405 httpContext
    | _ -> asyncNext
