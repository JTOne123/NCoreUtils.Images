namespace NCoreUtils.Images

open System
open System.IO
open System.Text
open Microsoft.Extensions.Logging
open NCoreUtils
open NCoreUtils.AspNetCore

module ValueOption = Microsoft.FSharp.Core.ValueOption

type GoogleCloudStorageSourceExtractor (logger : ILogger<GoogleCloudStorageSourceExtractor>) =
  interface IImageSourceExtractor with
    member __.AsyncExtractImageSource httpContext =
      let result =
        let headers = HttpContext.requestHeaders httpContext
        match Headers.tryGetFirstValue "X-SourceType" headers with
        | ValueSome (EQI "GoogleCloudStorageRecord") ->
          match Headers.tryGetFirstValue "X-SourceAccessToken" headers with
          | ValueSome accessToken ->
            logger.LogDebug "Detected GCS source."
            { new IImageSource with
                member __.AsyncCreateProducer () = async {
                  use  reader    = new StreamReader (httpContext.Request.Body, Encoding.ASCII)
                  let! uriString = Async.Adapt (fun _ -> reader.ReadToEndAsync ())
                  let  uri       = Uri (uriString, UriKind.Absolute)
                  logger.LogInformation ("Extracted GCS source: {0}", uriString)
                  return new GoogleStorageRecordProducer (uri, accessToken) :> _ }
            }
            |> ValueSome
          | _ ->
            logger.LogWarning "Detected GCS source but X-SourceAccessToken was not set."
            ValueNone
        | _ -> ValueNone
      async.Return result

type GoogleCloudStorageDestinationExtractor (logger : ILogger<GoogleCloudStorageDestinationExtractor>) =
  interface IImageDestinationExtractor with
    member __.AsyncExtractImageDestination httpContext =
      let result =
        let headers = HttpContext.requestHeaders httpContext
        match Headers.tryGetFirstValue "X-DestinationType" headers with
        | ValueSome (EQI "GoogleCloudStorageRecord") ->
          match Headers.tryGetFirstValue "X-DestinationAccessToken" headers with
          | ValueSome accessToken ->
            logger.LogDebug "Detected GCS destination."
            match Headers.tryGetFirstValue "X-DestinationUri" headers with
            | ValueSome uriString ->
              let contentType =
                Headers.tryGetFirstValue "X-DestinationContentType" headers
                |? null
              let cacheControl =
                Headers.tryGetFirstValue "X-DestinationCacheControl" headers
                |? null
              let isPublic =
                Headers.tryGetFirstValue "X-DestinationIsPublic" headers
                |> ValueOption.contains "true"
              let uri = Uri (uriString, UriKind.Absolute)
              logger.LogInformation (
                "Extracted GCS destination: {0}{1}{2}{3}",
                uriString,
                (if String.IsNullOrEmpty contentType  then String.Empty else sprintf ", %s" contentType),
                (if String.IsNullOrEmpty cacheControl then String.Empty else sprintf ", %s" cacheControl),
                (if isPublic                          then ", public"   else ", private")
              )
              new GoogleStorageRecordDestination (uri, ViaAccessToken accessToken, contentType, cacheControl, isPublic)
              :> IImageDestination
              |> ValueSome
            | _ ->
              logger.LogWarning "Detected GCS destination but X-DestinationUri was not set."
              ValueNone
          | _ ->
            logger.LogWarning "Detected GCS destination but X-DestinationAccessToken was not set."
            ValueNone
        | _ -> ValueNone
      async.Return result



