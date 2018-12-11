namespace NCoreUtils.Images.ImageSharp

open System
open System.Collections.Immutable
open System.IO
open System.Runtime.CompilerServices
open System.Threading
open NCoreUtils.Images
open NCoreUtils.Images.Internal
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Formats
open SixLabors.ImageSharp.Formats.Png
open SixLabors.ImageSharp.Formats.Jpeg
open SixLabors.ImageSharp.Formats.Gif
open SixLabors.ImageSharp.Formats.Bmp
open SixLabors.ImageSharp.Processing

type private S = SixLabors.Primitives.Size
type private R = SixLabors.Primitives.Rectangle

[<AutoOpen>]
module private WrapperHelpers =

  let name2ImageType =
    Map.ofList
      [ "PNG",  ImageType.Png
        "JPEG", ImageType.Jpeg
        "GIF",  ImageType.Gif
        "BMP",  ImageType.Bmp ]

  let inline imageTypeFromName name =
    match Map.tryFind name name2ImageType with
    | Some imageType -> imageType
    | _              -> ImageType.Other

  let inline pngFilterTypeFromInt i =
    match i with
    | 1 -> PngFilterMethod.Sub
    | 2 -> PngFilterMethod.Up
    | 3 -> PngFilterMethod.Average
    | 4 -> PngFilterMethod.Paeth
    | 5 -> PngFilterMethod.Adaptive
    | _ -> PngFilterMethod.None

  let inline createEncoder imageType quality =
    match imageType with
    | ImageType.Jpeg -> JpegEncoder (Quality = quality) :> IImageEncoder
    | ImageType.Gif  -> GifEncoder  () :> IImageEncoder
    | ImageType.Bmp  -> BmpEncoder  () :> IImageEncoder
    | ImageType.Png  ->
      let compressionLevel = quality / 10
      let filterType       = quality % 10
      PngEncoder  (CompressionLevel = compressionLevel, FilterMethod = pngFilterTypeFromInt filterType) :> IImageEncoder
    | _              -> sprintf "image type %A not supported" imageType |> NotSupportedException |> raise

type ImageProvider () =
  member this.FromStream (stream : Stream) =
    let mutable format = Unchecked.defaultof<_>
    let image = Image.Load (stream, &format)
    new Wrapper (image, format, this)
  member this.AsyncFromStream stream =
    Async.FromContinuations
      (fun (succ, err, _) ->
        try this.FromStream stream :> NCoreUtils.Images.Internal.IImage |> succ
        with e ->
          eprintfn "async error"
          eprintfn "%A" e
          err e
      )
  interface IImageProvider with
    member __.MemoryLimit
      with get () = 0L
      and  set _  = ()
    member this.AsyncFromStream stream = this.AsyncFromStream stream
    member this.AsyncResFromStream stream =
      let res =
        try this.FromStream stream :> NCoreUtils.Images.Internal.IImage |> Ok
        with
          | e -> Error <| ImageResizerError.generic "custom" e.Message
          // | _ -> reraise ()
      async.Return res

and
  [<Sealed>]
  Wrapper =
    val mutable private isDisposed : int
    val mutable private source     : Image<PixelFormats.Rgba32>
    val private provider   : ImageProvider
    val private format     : IImageFormat
    new (source, format, provider) =
      { isDisposed = 0
        provider   = provider
        source     = source
        format     = format }
    member private this.IsDisposed with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get () = 0 <> this.isDisposed
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member private this.ThrowIfDisposed () =
      if this.IsDisposed then
        ObjectDisposedException "Wrapper" |> raise
    // [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member private this.Process (action : Action<IImageProcessingContext<PixelFormats.Rgba32>>) =
      // FIMXE: race
      let clone = this.source.Clone action
      Interlocked.MemoryBarrier ()
      let oldImage = Interlocked.Exchange (&this.source, clone)
      oldImage.Dispose ()
    member this.Provider with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get () = this.provider
    member this.Size
      with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get () =
        { Width  = this.source.Width
          Height = this.source.Height }
    member this.ImageType
      with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get () =
        imageTypeFromName this.format.Name
    member this.WriteTo (stream : Stream, imageType : ImageType, quality) =
      this.ThrowIfDisposed ()
      let encoder = createEncoder imageType quality
      encoder.Encode (this.source, stream)
    member this.Resize (size : Size) =
      this.ThrowIfDisposed ()
      this.Process (fun x -> x.Resize (S (size.Width, size.Height)) |> ignore)
    member this.Crop (rect : Rectangle) =
      this.ThrowIfDisposed ()
      this.Process (fun x -> x.Crop(R (rect.X, rect.Y, rect.Width, rect.Height)) |> ignore)
    member this.GetImageInfo () : ImageInfo =
      this.ThrowIfDisposed ()
      let iptc = this.source.MetaData.IccProfile
      let exif = this.source.MetaData.ExifProfile
      let iptcBuilder = ImmutableDictionary.CreateBuilder ()
      let exifBuilder = ImmutableDictionary.CreateBuilder ()
      if not (isNull iptc) then
        for value in iptc.Entries do
          let tag = value.TagSignature.ToString ()
          if not (iptcBuilder.ContainsKey tag) then
            iptcBuilder.Add (tag, value.Signature.ToString ())
      if not (isNull exif) then
        for value in exif.Values do
          match value.Value with
          | null -> ()
          | v ->
            let tag = value.Tag.ToString ()
            if not (exifBuilder.ContainsKey tag) then
              exifBuilder.Add (tag, v.ToString ())
      let struct (xResolution, yResolution) =
        match this.source.MetaData.ResolutionUnits with
        | MetaData.PixelResolutionUnit.PixelsPerCentimeter -> struct (int (this.source.MetaData.HorizontalResolution * 2.54),   int (this.source.MetaData.VerticalResolution * 2.54))
        | MetaData.PixelResolutionUnit.PixelsPerMeter      -> struct (int (this.source.MetaData.HorizontalResolution * 0.0254), int (this.source.MetaData.VerticalResolution * 0.0254))
        | MetaData.PixelResolutionUnit.PixelsPerInch       -> struct (int this.source.MetaData.HorizontalResolution,            int this.source.MetaData.VerticalResolution)
        | _                                                -> struct (0, 0)
      { Width       = this.source.Width
        Height      = this.source.Height
        XResolution = xResolution
        YResolution = yResolution
        Iptc        = iptcBuilder.ToImmutable ()
        Exif        = exifBuilder.ToImmutable () }
    interface NCoreUtils.Images.Internal.IImage with
      member this.Provider  = this.provider :> _
      member this.Size      = this.Size
      member this.ImageType = this.ImageType
      member this.AsyncWriteTo (stream, imageType, quality) =
        Async.FromContinuations
          (fun (succ, err, _) ->
            try
              this.WriteTo (stream, imageType, quality)
              succ ()
            with exn -> err exn
          )
      member this.AsyncSaveTo (path, imageType, quality) =
        Async.FromContinuations
          (fun (succ, err, _) ->
            try
              this.SaveTo (path, imageType, quality)
              succ ()
            with exn -> err exn
          )
      member this.Resize size = this.Resize size
      member this.Crop rect = this.Crop rect
      member this.GetImageInfo () = this.GetImageInfo ()
      member this.Dispose () =
        if 0 = Interlocked.CompareExchange (&this.isDisposed, 1, 0) then
          this.source.Dispose ()
