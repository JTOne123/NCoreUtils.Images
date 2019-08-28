namespace NCoreUtils.Images

open System.Runtime.InteropServices
open System.Threading
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open NCoreUtils

module ValueOption = Microsoft.FSharp.Core.ValueOption

[<AbstractClass>]
type AsyncImageSourceExtractor () =
  abstract ExtractImageSourceAsync : httpContext:HttpContext * [<Optional>] cancelllationToken : CancellationToken -> Task<IImageSource>

  member inline internal this.ExtractImageSourceDirect (httpContext, cancellationToken) =
    this.ExtractImageSourceAsync (httpContext, cancellationToken)

  interface IImageSourceExtractor with
    member this.AsyncExtractImageSource httpContext = async {
      let! res = Async.Adapt (fun cancellationToken -> this.ExtractImageSourceAsync (httpContext, cancellationToken))
      return ValueOption.ofObj res }

[<AbstractClass>]
type AsyncImageDestinationExtractor () =
  abstract ExtractImageDestinationAsync : httpContext:HttpContext * [<Optional>] cancelllationToken : CancellationToken -> Task<IImageDestination>

  member inline internal this.ExtractImageDestinationDirect (httpContext, cancellationToken) =
    this.ExtractImageDestinationAsync (httpContext, cancellationToken)

  interface IImageDestinationExtractor with
    member this.AsyncExtractImageDestination httpContext = async {
      let! res = Async.Adapt (fun cancellationToken -> this.ExtractImageDestinationAsync (httpContext, cancellationToken))
      return ValueOption.ofObj res }
