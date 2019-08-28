namespace NCoreUtils.Images

open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Threading
open NCoreUtils

module ValueOption = Microsoft.FSharp.Core.ValueOption

[<Extension>]
[<AbstractClass; Sealed>]
type ImageSourceExtractorExtensions =

  [<Extension>]
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  static member ExtractImageSource (this : IImageSourceExtractor, httpContext) =
    match this with
    | :? AsyncImageSourceExtractor as inst ->
      inst.ExtractImageSourceDirect(httpContext, CancellationToken.None).Result
    | _ ->
      this.AsyncExtractImageSource httpContext |> Async.RunSynchronously |> ValueOption.toObj

  [<Extension>]
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  static member ExtractImageSourceAsync (this : IImageSourceExtractor, httpContext, [<Optional>] cancellationToken) =
    match this with
    | :? AsyncImageSourceExtractor as inst ->
      inst.ExtractImageSourceDirect (httpContext, cancellationToken)
    | _ ->
      Async.StartAsTask (this.AsyncExtractImageSource httpContext >>| ValueOption.toObj, cancellationToken = cancellationToken)

[<Extension>]
[<AbstractClass; Sealed>]
type ImageDestinationExtractorExtensions =

  [<Extension>]
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  static member ExtractImageDestination (this : IImageDestinationExtractor, httpContext) =
    match this with
    | :? AsyncImageDestinationExtractor as inst ->
      inst.ExtractImageDestinationDirect(httpContext, CancellationToken.None).Result
    | _ ->
      this.AsyncExtractImageDestination httpContext |> Async.RunSynchronously |> ValueOption.toObj

  [<Extension>]
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  static member ExtractImageDestinationAsync (this : IImageDestinationExtractor, httpContext, [<Optional>] cancellationToken) =
    match this with
    | :? AsyncImageDestinationExtractor as inst ->
      inst.ExtractImageDestinationDirect (httpContext, cancellationToken)
    | _ ->
      Async.StartAsTask (this.AsyncExtractImageDestination httpContext >>| ValueOption.toObj, cancellationToken = cancellationToken)
