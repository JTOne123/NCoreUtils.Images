namespace NCoreUtils.Images

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open NCoreUtils
open NCoreUtils.IO

module ValueOption = Microsoft.FSharp.Core.ValueOption

[<Struct>]
[<NoEquality; NoComparison>]
type private Buffer =
  | InMemory of MemoryStream:MemoryStream
  | InFile   of FileStream:FileStream

type private OffloadableBuffer =
  val private maxInMemorySize : int64
  val mutable private buffer : Buffer
  new (memoryStream, maxInMemorySize) =
    { buffer          = InMemory memoryStream
      maxInMemorySize = maxInMemorySize}
  new (memoryStream) = OffloadableBuffer (memoryStream, 16L * 1024L * 1024L)
  member this.Stream =
    match this.buffer with
    | InMemory stream -> stream :> Stream
    | InFile   stream -> stream :> _
  member this.AsyncWrite (data : byte[], offset, count : int) = async {
    match this.buffer with
    | InMemory buffer ->
      match buffer.Length + int64 count > this.maxInMemorySize with
      | false ->
        buffer.Write (data, offset, count)
      | true ->
        let newBuffer = new FileStream (Path.GetTempFileName (), FileMode.Create, FileAccess.ReadWrite, FileShare.None, 8192, true)
        do! Stream.asyncCopyTo newBuffer buffer
        this.buffer <- InFile newBuffer
        buffer.Dispose ()
        return! this.AsyncWrite (data, offset, count)
    | InFile buffer ->
      do! buffer.AsyncWrite (data, offset, count) }

[<Struct>]
[<NoEquality; NoComparison>]
type private AtmCallArgs = {
  data : byte[]
  offset : int
  count  : int }


and private OffloadableStream (maxInMemorySize) =
  inherit Stream ()
  let buffer = OffloadableBuffer (new MemoryStream (), maxInMemorySize)

  let (beginAction, endAction, cancellAction) =
    Async.AsBeginEnd (fun (args : AtmCallArgs) -> buffer.AsyncWrite (args.data, args.offset, args.count))

  member __.Buffer = buffer.Stream
  override __.CanRead with [<ExcludeFromCodeCoverage>] get () = false
  override __.CanSeek with [<ExcludeFromCodeCoverage>] get () = false
  override __.CanTimeout with [<ExcludeFromCodeCoverage>] get () = false
  override __.CanWrite = true
  override this.Length       with [<ExcludeFromCodeCoverage>] get () = this.Buffer.Length
  override this.Position     with [<ExcludeFromCodeCoverage>] get () = this.Buffer.Position     and [<ExcludeFromCodeCoverage>] set _value = NotSupportedException () |> raise
  override this.ReadTimeout  with [<ExcludeFromCodeCoverage>] get () = this.Buffer.ReadTimeout  and [<ExcludeFromCodeCoverage>] set _value = NotSupportedException () |> raise
  override this.WriteTimeout with [<ExcludeFromCodeCoverage>] get () = this.Buffer.WriteTimeout and [<ExcludeFromCodeCoverage>] set _value = NotSupportedException () |> raise
  [<ExcludeFromCodeCoverage>]
  override __.BeginWrite (buffer, offset, count, callback, state) =
    beginAction ({ data = buffer; offset = offset; count = count }, callback, state)
  [<ExcludeFromCodeCoverage>]
  override __.Close () = ()
  [<ExcludeFromCodeCoverage>]
  override this.CopyToAsync (destination, bufferSize, cancellationToken) = this.Buffer.CopyToAsync (destination, bufferSize, cancellationToken)
  [<ExcludeFromCodeCoverage>]
  override __.Dispose _disposing = ()
  [<ExcludeFromCodeCoverage>]
  override __.EndWrite asyncResult = endAction asyncResult
  [<ExcludeFromCodeCoverage>]
  override this.Flush () =
    this.Buffer.Flush ()
  [<ExcludeFromCodeCoverage>]
  override this.FlushAsync cancellationToken =
    this.Buffer.FlushAsync cancellationToken
  [<ExcludeFromCodeCoverage>]
  override __.SetLength _value = NotSupportedException () |> raise
  [<ExcludeFromCodeCoverage>]
  override this.Write (buffer, offset, count) =
    this.WriteAsync(buffer, offset, count).Wait ()
  override __.WriteAsync (data, offset, count, cancellationToken) =
    Async.StartAsTask (buffer.AsyncWrite (data, offset, count), cancellationToken = cancellationToken) :> _
  [<ExcludeFromCodeCoverage>]
  override this.WriteByte value =
    this.Write ([| value |], 0, 1)
  [<ExcludeFromCodeCoverage>]
  override __.Read (_, _, _) = NotSupportedException () |> raise
  [<ExcludeFromCodeCoverage>]
  override __.Seek (_, _) = NotSupportedException () |> raise

type internal BufferedStreamProducer (source : IStreamProducer, maxInMemorySize : int) =
  let mutable target = ValueNone

  new (source) = new BufferedStreamProducer (source, 16 * 1024 * 1024)

  interface IStreamProducer with
    member __.AsyncProduce (output: Stream) = async {
      if ValueOption.isNone target then
        let offloadableStream = new OffloadableStream (int64 maxInMemorySize)
        do! source.AsyncProduce offloadableStream
        target <- ValueSome offloadableStream.Buffer
      let bufferedSource = ValueOption.get target
      bufferedSource.Seek (0L, SeekOrigin.Begin) |> ignore
      do! Stream.asyncCopyTo output bufferedSource }
    member __.Dispose () =
      match target with
      | ValueNone -> ()
      | ValueSome stream -> stream.Dispose ()
