namespace NCoreUtils.Images.ImageSharp

open System.IO
open SixLabors.ImageSharp

type ImageProvider () =
  member this.FromStream (stream : Stream) = new Wrapper (Image.Load stream, this)
  interface IImageProvider with
    member __.MemoryLimit
      with get () = 0L
      and  set _  = ()
    member this.AsyncFromStream stream =
      Async.FromContinuations
        (fun (succ, err, _) ->
          try this.FromStream stream :> IImage |> succ
          with e -> err e
        )
    member this.AsyncResFromStream stream =
      let res =
        try this.FromStream stream :> IImage |> Ok
        with
          | e -> Error e.Message
          | _ -> reraise ()
      async.Return res

  interface IDirectImageProvider with
    member this.AsyncFromPath path =
      new Wrapper (Image.Load path, this) :> IImage |> async.Return
    member this.AsyncResFromPath path =
      let res =
        try new Image (new MagickImage (path), this) :> IImage |> Ok
        with
          | e -> Error e.Message
          | _ -> reraise ()
      async.Return res


and
  [<Sealed>]
  Wrapper =
    val private provider   : ImageProvider

    val private source     : Image<PixelFormats.Rgba32>
    new (source, provider) =
      { provider = provider
        source   = source }