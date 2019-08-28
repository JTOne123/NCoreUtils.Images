namespace NCoreUtils.Images.Internal

open System
open System.IO
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Threading
open System.Threading.Tasks
open NCoreUtils
open NCoreUtils.Images
open NCoreUtils.Images

type
  [<Struct>]
  [<CustomEquality; NoComparison>]
  Size = {
    Width  : int
    Height : int }
  with
    static member inline private Eq (a : Size, b : Size) = a.Width = b.Width && a.Height = b.Height
    static member Empty = { Width = 0; Height = 0 }
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member op_Equality (a, b) = Size.Eq (a, b)
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member op_Inequality (a, b) = not <| Size.Eq (a, b)
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.Equals other = Size.Eq (this, other)
    interface IEquatable<Size> with
      member this.Equals other = Size.Eq (this, other)
    override this.Equals obj =
      match obj with
      | :? Size as other -> Size.Eq (this, other)
      | _                -> false
    override this.GetHashCode () = this.Width ^^^ this.Height

type
  [<Struct>]
  [<CustomEquality; NoComparison>]
  Point = {
    X : int
    Y : int }
  with
    static member inline private Eq (a : Point, b : Point) = a.X = b.X && a.Y = b.Y
    static member Empty = { X = 0; Y = 0 }
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member op_Equality (a, b) = Point.Eq (a, b)
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member op_Inequality (a, b) = not <| Point.Eq (a, b)
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.Equals other = Point.Eq (this, other)
    interface IEquatable<Point> with
      member this.Equals other = Point.Eq (this, other)
    override this.Equals obj =
      match obj with
      | :? Point as other -> Point.Eq (this, other)
      | _                 -> false
    override this.GetHashCode () = this.X ^^^ this.Y

type
  [<Struct>]
  [<CustomEquality; NoComparison>]
  Rectangle = {
    Point : Point
    Size  : Size }
  with
    static member inline private Eq (a : Rectangle, b : Rectangle) = a.Point = b.Point && a.Size = b.Size
    static member Empty = { Point = Point.Empty; Size = Size.Empty }
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member op_Equality (a, b) = Rectangle.Eq (a, b)
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member op_Inequality (a, b) = not <| Rectangle.Eq (a, b)
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.Equals other = Rectangle.Eq (this, other)
    interface IEquatable<Rectangle> with
      member this.Equals other = Rectangle.Eq (this, other)
    override this.Equals obj =
      match obj with
      | :? Rectangle as other -> Rectangle.Eq (this, other)
      | _                 -> false
    override this.GetHashCode () = this.Point.GetHashCode () ^^^ this.Size.GetHashCode ()
    member this.X      with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get () = this.Point.X
    member this.Y      with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get () = this.Point.Y
    member this.Width  with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get () = this.Size.Width
    member this.Height with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get () = this.Size.Height
    member this.Top    with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get () = this.Y
    member this.Left   with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get () = this.X
    member this.Bottom with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get () = this.Y + this.Height
    member this.Right  with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get () = this.X + this.Width

type ImageType =
  /// <summary>
  /// JPG
  /// </summary>
  | Jpeg = 0
  /// <summary>
  /// PNG
  /// </summary>
  | Png = 1
  /// <summary>
  /// BMP
  /// </summary>
  | Bmp = 2
  /// <summary>
  /// GIF
  /// </summary>
  | Gif = 3
  /// <summary>
  /// TIF
  /// </summary>
  | Tiff = 4
  /// WEBP
  | Webp = 5
  /// <summary>
  /// Other
  /// </summary>
  | Other = -1

[<CompiledName("ImageTypes")>]
[<RequireQualifiedAccess>]
[<Extension>]
module ImageType =

  let private ext2type =
    Map.ofList
      [ CaseInsensitive "jpg", ImageType.Jpeg
        CaseInsensitive "jpeg", ImageType.Jpeg
        CaseInsensitive "bmp", ImageType.Bmp
        CaseInsensitive "png", ImageType.Png
        CaseInsensitive "gif", ImageType.Gif
        CaseInsensitive "tiff", ImageType.Tiff ]

  let private type2ext =
    Map.ofList
      [ ImageType.Jpeg, "jpg"
        ImageType.Bmp,  "bmp"
        ImageType.Png,  "png"
        ImageType.Gif,  "gif"
        ImageType.Tiff, "tiff" ]

  let private type2mime =
    Map.ofList
      [ ImageType.Jpeg, "image/jpeg"
        ImageType.Bmp,  "image/bmp"
        ImageType.Png,  "image/png"
        ImageType.Gif,  "image/gif"
        ImageType.Tiff, "image/tiff" ]

  [<CompiledName("OfExtension")>]
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let ofExtension ext = Map.tryFind (CaseInsensitive ext) ext2type |> Option.defaultValue ImageType.Other

  [<CompiledName("ToExtension")>]
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  [<Extension>]
  let toExtension imageType =
    match Map.tryFind imageType type2ext with
    | Some ext -> ext
    | _        -> invalidArgf "imageType" "Not valid image type (%A)" imageType

  [<CompiledName("ToMediaType")>]
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  [<Extension>]
  let toMediaType imageType =
    match Map.tryFind imageType type2mime with
    | Some ext -> ext
    | _        -> invalidArgf "imageType" "Not valid image type (%A)" imageType


type
  [<Interface>]
  IImageProvider =
    abstract MemoryLimit : int64 with get, set
    abstract AsyncFromStream    : stream:Stream -> Async<IImage>
    abstract AsyncResFromStream : stream:Stream -> Async<Result<IImage, ImageResizerError>>

and
  [<Interface>]
  IImage =
    inherit IDisposable
    abstract Provider        : IImageProvider
    abstract Size            : Size
    abstract NativeImageType : obj
    abstract ImageType       : ImageType
    abstract AsyncWriteTo    : stream:Stream * imageType:ImageType * [<Optional; DefaultParameterValue(85)>] quality:int * [<Optional; DefaultParameterValue(true)>] optimize:bool -> Async<unit>
    abstract AsyncSaveTo     : path:string   * imageType:ImageType * [<Optional; DefaultParameterValue(85)>] quality:int * [<Optional; DefaultParameterValue(true)>] optimize:bool -> Async<unit>
    abstract Resize          : size:Size      -> unit
    abstract Crop            : rect:Rectangle -> unit
    abstract GetImageInfo    : unit -> ImageInfo

type
  [<Interface>]
  IDirectImageProvider =
    inherit IImageProvider
    abstract AsyncFromPath : path:string -> Async<IImage>
    abstract AsyncResFromPath : path:string -> Async<Result<IImage, ImageResizerError>>


type
  [<Interface>]
  IDirectImage =
    inherit IImage
    abstract AsyncSaveTo : path:string * [<Optional; DefaultParameterValue(85)>] quality:int -> Async<unit>

// C# interop

type
  [<AbstractClass>]
  AsyncImageProvider () =
    abstract MemoryLimit : int64 with get, set

    abstract FromStreamAsync : stream:Stream * [<Optional>] cancellationToken:CancellationToken -> Task<IImage>
    abstract TryFromStreamAsync : stream:Stream * [<Optional>] cancellationToken:CancellationToken -> Task<AsyncResult<IImage, ImageResizerError>>
    member inline internal this.FromStreamDirect (stream, cancellationToken) = this.FromStreamAsync (stream, cancellationToken)
    interface IImageProvider with
      member this.MemoryLimit with get () = this.MemoryLimit and set limit = this.MemoryLimit <- limit
      member this.AsyncFromStream stream = Async.Adapt (fun cancellationToken -> this.FromStreamAsync (stream, cancellationToken))
      member this.AsyncResFromStream stream =
        Async.Adapt (fun cancellationToken -> this.TryFromStreamAsync (stream, cancellationToken))
        >>| AsyncResult<_, _>.ToResult

type
  [<AbstractClass>]
  AsyncImage =
    val mutable private provider : IImageProvider
    new (provider) = { provider = provider }
    member this.Provider with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get () = this.provider
    abstract Size            : Size
    abstract NativeImageType : obj
    abstract ImageType       : ImageType
    abstract WriteToAsync    : stream:Stream * imageType:ImageType * [<Optional; DefaultParameterValue(85)>] quality:int * [<Optional; DefaultParameterValue(true)>] optimize:bool * [<Optional>] cancellationToken:CancellationToken -> Task
    abstract SaveToAsync     : path:string   * imageType:ImageType * [<Optional; DefaultParameterValue(85)>] quality:int * [<Optional; DefaultParameterValue(true)>] optimize:bool * [<Optional>] cancellationToken:CancellationToken -> Task
    abstract Resize          : size:Size      -> unit
    abstract Crop            : rect:Rectangle -> unit
    abstract GetImageInfo    : unit -> ImageInfo
    abstract Dispose         : disposing:bool -> unit
    member inline internal this.WriteToDirect (stream, imageType, quality, optimize, cancellationToken) = this.WriteToAsync (stream, imageType, quality, optimize, cancellationToken)
    member inline internal this.SaveToDirect (path, imageType, quality, optimize, cancellationToken) = this.SaveToAsync (path, imageType, quality, optimize, cancellationToken)
    interface IImage with
      member this.Provider        = this.Provider
      member this.Size            = this.Size
      member this.NativeImageType = this.NativeImageType
      member this.ImageType       = this.ImageType
      member this.AsyncWriteTo (stream, imageType, quality, optimize) = Async.Adapt (fun cancellationToken -> this.WriteToAsync (stream, imageType, quality, optimize, cancellationToken))
      member this.AsyncSaveTo  (path, imageType, quality, optimize) = Async.Adapt (fun cancellationToken -> this.SaveToAsync (path, imageType, quality, optimize, cancellationToken))
      member this.Resize size = this.Resize size
      member this.Crop   rect = this.Crop   rect
      member this.GetImageInfo () = this.GetImageInfo ()
      member this.Dispose () =
        GC.SuppressFinalize this
        this.Dispose true

[<AutoOpen>]
module ImageExt =

  type IImageProvider with
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.AsyncFromData (data : byte[]) = async {
      use buffer = new MemoryStream (data, false)
      return! this.AsyncFromStream buffer }


[<Extension>]
[<Sealed; AbstractClass>]
type ImageExtensions =

  [<Extension>]
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  static member FromStream (this : IImageProvider, stream) =
    this.AsyncFromStream stream |> Async.RunSynchronously

  [<Extension>]
  static member FromStreamAsync (this : IImageProvider, stream, [<Optional>] cancellationToken) =
    match this with
    | :? AsyncImageProvider as inst -> inst.FromStreamDirect (stream, cancellationToken)
    | _                             -> Async.StartAsTask (this.AsyncFromStream stream, cancellationToken = cancellationToken)

  [<Extension>]
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  static member FromData (this : IImageProvider, data) =
    use buffer = new MemoryStream (data, false)
    this.FromStream buffer

  [<Extension>]
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  static member FromDataAsync (this : IImageProvider, data, [<Optional>] cancellationToken) =
    Async.StartAsTask (this.AsyncFromData data, cancellationToken = cancellationToken)


  [<Extension>]
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  static member WriteTo (this : IImage, stream, imageType, [<Optional; DefaultParameterValue(85)>] quality, [<Optional; DefaultParameterValue(true)>] optimize) =
    this.AsyncWriteTo (stream, imageType, quality, optimize) |> Async.RunSynchronously

  [<Extension>]
  static member WriteToAsync (this : IImage, stream, imageType, [<Optional; DefaultParameterValue(85)>] quality, [<Optional; DefaultParameterValue(true)>] optimize, [<Optional>] cancellationToken) =
    match this with
    | :? AsyncImage as inst -> inst.WriteToDirect (stream, imageType, quality, optimize, cancellationToken)
    | _                     -> Async.StartAsTask (this.AsyncWriteTo (stream, imageType, quality, optimize), cancellationToken = cancellationToken) :> _

  [<Extension>]
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  static member SaveTo (this : IImage, path, imageType, [<Optional; DefaultParameterValue(85)>] quality) =
    this.AsyncSaveTo (path, imageType, quality) |> Async.RunSynchronously

  [<Extension>]
  static member SaveToAsync (this : IImage, path, imageType, [<Optional; DefaultParameterValue(85)>] quality, [<Optional; DefaultParameterValue(true)>] optimize, [<Optional>] cancellationToken) =
    match this with
    | :? AsyncImage as inst -> inst.SaveToDirect (path, imageType, quality, optimize, cancellationToken)
    | _                     -> Async.StartAsTask (this.AsyncSaveTo (path, imageType, quality, optimize), cancellationToken = cancellationToken) :> _

// ********************************************************************************************************************
// Resizing

type
  [<Interface>]
  IResizer =
    abstract Resize : image:IImage -> unit

type
  [<Interface>]
  IResizerFactory =
    abstract CreateResizer : image:IImage * options:ResizeOptions -> IResizer

type
  [<Sealed>]
  NoneResizerFactory () =
    static let resizer =
      { new IResizer with
          member __.Resize _ = ()
      }
    interface IResizerFactory with
      member __.CreateResizer (_, _) = resizer

type
  [<Sealed>]
  ExactResizerFactory () =
    interface IResizerFactory with
      member __.CreateResizer (image, options) =
        let box =
          match options.Width.HasValue, options.Height.HasValue with
          | false, false -> invalidOp "Output image dimensions must be specified when using exact resizing."
          | true,  false ->
            let imageSize = image.Size
            { Width  = options.Width.Value
              Height = int (double imageSize.Height / double imageSize.Width * double options.Width.Value) }
          | false, true ->
            let imageSize = image.Size
            { Width  = int (double imageSize.Width / double imageSize.Height * double options.Height.Value)
              Height = options.Height.Value }
          | _ -> { Width = options.Width.Value; Height = options.Height.Value }
        { new IResizer with member __.Resize image = image.Resize box }

type
  [<Sealed>]
  InboxResizerFactory () =
    static let calculateRectangle (weightX : Nullable<int>) (weightY : Nullable<int>) (source : Size) (box : Size) =
      let resizeWidthRatio  = double source.Width  / double box.Width
      let resizeHeightRatio = double source.Height / double box.Height
      match resizeWidthRatio < resizeHeightRatio with
      | true ->
        // maximize width in box
        let inputHeight = int (double box.Height / double box.Width * double source.Width)
        let mutable margin = (source.Height - inputHeight) / 2
        if weightY.HasValue then
          let diff = double weightY.Value - double source.Height / 2.0;
          let normalized = diff / (double source.Height / 2.0);
          let mul = 1.0 + normalized;
          margin <- int (double margin * mul)
        { Point = { X = 0; Y = margin }; Size = { Width = source.Width; Height = inputHeight } }
      | _ ->
        let inputWidth = int (double box.Width / double box.Height * double source.Height)
        let mutable margin = (source.Width - inputWidth) / 2
        if weightX.HasValue then
          let diff = double weightX.Value - double source.Width / 2.0;
          let normalized = diff / (double source.Width / 2.0)
          let mul = 1.0 + normalized
          margin <- int (double margin * mul);
        { Point = { X = margin; Y = 0 }; Size = { Width = inputWidth; Height = source.Height } }
    interface IResizerFactory with
      member __.CreateResizer (_, options) =
        if not (options.Width.HasValue && options.Height.HasValue) then
          invalidOp "Exact image dimensions must be specified when using inbox resizing."
        let box = { Width = options.Width.Value; Height = options.Height.Value }
        { new IResizer with
            member __.Resize image =
              let inputRect = calculateRectangle options.WeightX options.WeightY image.Size box
              image.Crop inputRect
              image.Resize box
        }

type
  [<Extension>]
  [<Sealed; AbstractClass>]
  ImageResizeExtensions =

    [<Extension>]
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member Resize (this : IImage, resizer : IResizer) = resizer.Resize this

[<Interface>]
type IOptimizer =
  abstract AsyncOptimize : data:byte[] -> Async<byte[]>
