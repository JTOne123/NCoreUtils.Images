namespace NCoreUtils.Images

open System
open System.IO
open System.Runtime.InteropServices
open System.Threading
open System.Threading.Tasks
open NCoreUtils
open NCoreUtils.IO
open System.Collections.Generic

[<Interface>]
type ILog =
  abstract LogDebug   : exn:exn * message:string -> unit
  abstract LogWarning : exn:exn * message:string -> unit
  abstract LogError   : exn:exn * message:string -> unit

[<Interface>]
type ILog<'a> = inherit ILog

[<Interface>]
[<AllowNullLiteral>]
type IImageSource =
  abstract AsyncCreateProducer : unit -> Async<IStreamProducer>

[<Interface>]
[<AllowNullLiteral>]
type IImageDestination =
  abstract AsyncWrite        : contentInfo:ContentInfo * generator:(Stream -> Async<unit>) -> Async<unit>
  abstract AsyncWriteDelayed : generator:((ContentInfo -> unit) -> Stream -> Async<unit>) -> Async<unit>

/// Extends image destination functinality with serialized propagation.
[<Interface>]
[<AllowNullLiteral>]
type ISerializableImageDestination =
  inherit IImageDestination
  /// Gets serialized information about the destination.
  abstract AsyncSerialize : unit -> Async<IReadOnlyDictionary<string, string>>

[<Interface>]
type IImageResizer =
  abstract AsyncResize       : source:IStreamProducer * destination:IImageDestination * options:ResizeOptions -> Async<unit>
  abstract AsyncResResize    : source:IStreamProducer * destination:IImageDestination * options:ResizeOptions -> Async<Result<unit, ImageResizerError>>
  abstract AsyncGetImageInfo : source:IStreamProducer -> Async<ImageInfo>

/// Represent serializable information about the image.
type SerializableImageDescription = {
  /// Binary part of the serializable information about the image.
  Body : byte[]
  /// Readable part of the serializable information about the image.
  Info : IReadOnlyDictionary<string, string> }

type ISerializableImageProducer =
  inherit IStreamProducer
  abstract AsyncSerialize : unit -> Async<SerializableImageDescription>

[<Interface>]
type IImageOptimization =
  inherit IStreamTransformation
  abstract Supports : imageType:string -> bool

// C# interop

[<AbstractClass>]
type AsyncImageSource () =
  abstract CreateProducerAsync
    : [<Optional>] cancellationToken:CancellationToken
    -> Task<IStreamProducer>

  member inline internal this.CreateProducerDirect cancellationToken =
    this.CreateProducerAsync cancellationToken

  interface IImageSource with
    member this.AsyncCreateProducer () =
      Async.Adapt (fun cancellationToken -> this.CreateProducerAsync cancellationToken)

[<AbstractClass>]
type AsyncImageDestination () =
  abstract WriteAsync
    :  contentInfo:ContentInfo
    *  generator:Func<Stream, CancellationToken, Task>
    *  cancellationToken:CancellationToken
    -> Task

  abstract WriteDelayedAsync
    :  generator:Func<Action<ContentInfo>, Stream, CancellationToken, Task>
    *  cancellationToken:CancellationToken
    -> Task

  member inline internal this.WriteDirect (contentInfo, generator, cancellationToken) =
    this.WriteAsync (contentInfo, generator, cancellationToken)

  member inline internal this.WriteDelayedDirect (generator, cancellationToken) =
    this.WriteDelayedAsync (generator, cancellationToken)

  interface IImageDestination with
    member this.AsyncWrite (contentInfo, generator) =
      let gen =
        Func<Stream, CancellationToken, Task>
          (fun stream cancellationToken ->
            Async.StartAsTask (generator stream, cancellationToken = cancellationToken) :> Task)
      Async.Adapt (fun cancellationToken -> this.WriteAsync (contentInfo, gen, cancellationToken))

    member this.AsyncWriteDelayed generator =
      let gen =
        Func<Action<ContentInfo>, Stream, CancellationToken, Task>
          (fun action stream cancellationToken ->
            Async.StartAsTask (generator action.Invoke stream, cancellationToken = cancellationToken) :> Task)
      Async.Adapt (fun cancellationToken -> this.WriteDelayedAsync (gen, cancellationToken))

[<AbstractClass>]
type AsyncImageResizer () =
  abstract ResizeAsync
    :  source:IStreamProducer
    *  destination:IImageDestination
    *  options:ResizeOptions
    *  cancellationToken:CancellationToken
    -> Task

  abstract TryResizeAsync
    :  source:IStreamProducer
    *  destination:IImageDestination
    *  options:ResizeOptions
    *  cancellationToken:CancellationToken
    -> Task<AsyncResult<ImageResizerError>>

  abstract GetImageInfoAsync
    :  source:IStreamProducer
    *  cancellationToken:CancellationToken
    -> Task<ImageInfo>

  member inline internal this.ResizeDirect (source : IStreamProducer, destination, options, cancellationToken) =
    this.ResizeAsync (source, destination, options, cancellationToken)

  member inline internal this.TryResizeDirect (source : IStreamProducer, destination, options, cancellationToken) =
    this.TryResizeAsync (source, destination, options, cancellationToken)

  member inline internal this.GetImageInfoDirect (source, cancellationToken) =
    this.GetImageInfoAsync (source, cancellationToken)

  interface IImageResizer with
    member this.AsyncResize (source : IStreamProducer, destination, options) =
      Async.Adapt (fun cancellationToken -> this.ResizeAsync (source, destination, options, cancellationToken))

    member this.AsyncResResize (source : IStreamProducer, destination, options) =
      Async.Adapt (fun cancellationToken -> this.TryResizeAsync (source, destination, options, cancellationToken))
      >>| AsyncResult.ToResult

    member this.AsyncGetImageInfo source = Async.Adapt (fun cancellationToken -> this.GetImageInfoAsync (source, cancellationToken))
