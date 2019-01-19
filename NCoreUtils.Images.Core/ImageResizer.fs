namespace NCoreUtils.Images

open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Runtime.Serialization
open NCoreUtils
open NCoreUtils.Images.Internal
open System.IO
open NCoreUtils.IO
open System.Threading.Tasks

type private LazyStream (source : ContentInfo -> Stream) = class end

module private InvalidResizeModeExceptionHelpers =
  [<Literal>]
  let KeyMode = "ResizeMode"

type InvalidResizeModeException =
  inherit ImageResizerException
  val private mode : string
  new (mode : string) =
    { inherit ImageResizerException (ImageResizerError.invalidMode mode)
      mode = mode }
  new (info : SerializationInfo, context) =
    { inherit ImageResizerException (info, context)
      mode = info.GetString InvalidResizeModeExceptionHelpers.KeyMode }
  member this.ResizeMode with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get () = this.mode
  override this.GetObjectData (info, context) =
    base.GetObjectData (info, context)
    info.AddValue (InvalidResizeModeExceptionHelpers.KeyMode, this.mode)

module private InvalidImageTypeExceptionHelpers =
  [<Literal>]
  let KeyImageType = "ImageType"

type InvalidImageTypeException =
  inherit ImageResizerException
  val private imageType : string
  new (imageType : string) =
    { inherit ImageResizerException (ImageResizerError.invalidImageType imageType)
      imageType = imageType }
  new (info : SerializationInfo, context) =
    { inherit ImageResizerException (info, context)
      imageType = info.GetString InvalidImageTypeExceptionHelpers.KeyImageType }
  member this.ImageType with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get () = this.ImageType
  override this.GetObjectData (info, context) =
    base.GetObjectData (info, context)
    info.AddValue (InvalidImageTypeExceptionHelpers.KeyImageType, this.imageType)

[<AutoOpen>]
module private ImageResizerHelpers =

  let inline private mkTemp (ext : string) = sprintf "%s%s.%s" (Path.GetTempPath ()) (System.Guid.NewGuid().ToString()) ext;

  let asyncTempFile (ext : string) action = async {
    let path = mkTemp ext
    try return! action path
    finally try if File.Exists path then File.Delete path with _ -> () }

  let inline getResult (res : Result<_, ImageResizerError>) =
    match res with
    | Ok result -> result
    | Error err -> err.RaiseException ()

  let inline decideImageType (image : IImage) (options : ResizeOptions) =
    match options.ImageType with
    | null  -> Ok image.ImageType
    | itype ->
    match ImageType.ofExtension itype with
    | ImageType.Other -> itype |> ImageResizerError.invalidImageType :> ImageResizerError |> Error
    | imageType       -> Ok imageType

type ImageResizer =
  val private provider      : IImageProvider
  val private resizers      : ResizerCollection
  val private options       : IImageResizerOptions
  val private logger        : ILog
  val private optimizations : seq<IImageOptimization>

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  new  (provider : IImageProvider,
        resizers,
        logger : ILog<ImageResizer>,
        [<Optional; DefaultParameterValue(null:IImageResizerOptions)>] options : IImageResizerOptions,
        [<Optional; DefaultParameterValue(null:seq<IImageOptimization>)>] optimizations : seq<IImageOptimization>) =
    let opts = if isNull options then (ImageResizerOptions.Default :> IImageResizerOptions) else options
    if opts.MemoryLimit.HasValue then
      provider.MemoryLimit <- opts.MemoryLimit.Value
    { provider      = provider
      resizers      = resizers
      options       = opts
      logger        = logger
      optimizations = optimizations }

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  member private this.TryGetOptimizationsFor shouldOptimize imageType =
    match shouldOptimize with
    | false -> []
    | _ ->
      let ext = ImageType.toExtension imageType
      let inline supports (o : IImageOptimization) = o.Supports ext
      this.optimizations
      |> Seq.filter supports
      |> Seq.toList

  member inline private this.DecideQuality (options : ResizeOptions) imageType =
    match options.Quality.HasValue with
    | true -> options.Quality.Value
    | _    -> this.options.Quality imageType

  member inline private this.DecideOptimize (options : ResizeOptions) imageType =
    match options.Optimize.HasValue with
    | true -> options.Optimize.Value
    | _    -> this.options.Optimize imageType

  member private this.TryGetOptimizationTransformation options imageType =
    let shouldOptimize = this.DecideOptimize options imageType
    match this.TryGetOptimizationsFor shouldOptimize imageType with
    | [] -> ValueNone
    | optimizations ->
      optimizations
      |> Seq.map (fun optimization -> optimization :> IStreamTransformation)
      |> Seq.reduce StreamTransformation.chain
      |> ValueSome

  member private this.CreateTransformation (options : ResizeOptions) =
    this.logger.LogDebug (null, sprintf "Processing options = %A" options)
    let resizeMode = options.ResizeMode |?? "none"
    match this.resizers.TryGetValue resizeMode with
    | ValueNone -> (ImageResizerError.invalidMode resizeMode).RaiseException ()
    | ValueSome factory ->
      // let resizeTransformation =
        { new IDependentStreamTransformation<_> with
            member __.Dispose () = ()
            member __.AsyncPerform (input, dependentOutput) = async {
              use! image = this.provider.AsyncFromStream input
              let imageType =
                match decideImageType image options with
                | Error err    -> err.RaiseException ()
                | Ok imageType -> imageType
              // setContentType <| ImageType.toMediaType imageType
              let output = dependentOutput imageType
              let resizer = factory.CreateResizer (image, options)
              image.Resize resizer
              let quality = this.DecideQuality options imageType
              do! image.AsyncWriteTo (output, imageType, quality)
              do! output.AsyncFlush ()
              output.Close () }
        }

  member this.AsyncResize (input : Stream, destination : IImageDestination, options) =
    destination.AsyncWriteDelayed
      (fun setContentInfo output -> async {
        let resizeTransformation = this.CreateTransformation options
        use pipeline =
          StreamTransformation.chainDependent
            resizeTransformation
            (fun imageType ->
              setContentInfo <| ContentInfo (contentType = ImageType.toMediaType imageType)
              this.TryGetOptimizationTransformation options imageType
            )
        do! pipeline.AsyncPerform (input, output)
        do! output.AsyncFlush ()
      })

  member this.AsyncResResize (input : Stream, destination : IImageDestination, options) = async {
    try
      do! this.AsyncResize (input, destination, options)
      return Ok ()
    with e ->
      return ImageResizerError.generic (e.GetType().Name) e.Message |> Error }

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  member this.AsyncGetImageInfo stream = async {
    this.logger.LogDebug (null, "Getting image information.")
    use! image = this.provider.AsyncFromStream stream
    let info = image.GetImageInfo ()
    printfn "%A" info
    this.logger.LogDebug (null, "Successfully retrieved image information.")
    return info }

  interface IImageResizer with
    member this.AsyncResize    (source : Stream, destination, options) = this.AsyncResize    (source, destination, options)
    member this.AsyncResResize (source : Stream, destination, options) = this.AsyncResResize (source, destination, options)
    member this.AsyncGetImageInfo stream = this.AsyncGetImageInfo stream