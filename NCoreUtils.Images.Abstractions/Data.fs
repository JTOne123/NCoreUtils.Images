namespace NCoreUtils.Images

open System
open System.Runtime.InteropServices
open System.Text
open System.Collections.Immutable

type
  [<Sealed>]
  [<AllowNullLiteral>]
  ContentInfo ([<OptionalAttribute>] contentLength : Nullable<int64>, [<OptionalAttribute>] contentType : string) =
  static member val Empty = ContentInfo ()
  member val ContentLength = contentLength
  member val ContentType   = contentType
  member this.WithContentLength contentLength = ContentInfo (contentLength, this.ContentType)
  member this.WithContentType   contentType   = ContentInfo (this.ContentLength, contentType)

[<AutoOpen>]
module private ResizeOptionsHelpers =

  let inline appends first (name : string) (value : string) (builder : StringBuilder) =
    match isNull value with
    | true -> builder
    | _ ->
      match !first with
      | true ->
        first := false
        builder.AppendFormat("{0} = {1}", name, value)
      | _ ->
        builder.AppendFormat(", {0} = {1}", name, value)

  let inline appendn first (name : string) (value : Nullable<_>) (builder : StringBuilder) =
    match value.HasValue with
    | false -> builder
    | _ ->
      match !first with
      | true ->
        first := false
        builder.AppendFormat("{0} = {1}", name, value.Value)
      | _ ->
        builder.AppendFormat(", {0} = {1}", name, value.Value)


  let inline builderStart (builder : StringBuilder) = builder.Append '['

  let inline builderEnd (builder : StringBuilder) = builder.Append ']'

  let inline builderToString (builder : StringBuilder) = builder.ToString ()

type
  [<AllowNullLiteral>]
  [<Sealed>]
  [<StructuredFormatDisplay("{DisplayValue}")>]
  ResizeOptions  ([<Optional; DefaultParameterValue(null:string)>] imageType  : string,
                  [<Optional>] width      : Nullable<int>,
                  [<Optional>] height     : Nullable<int>,
                  [<Optional>] resizeMode : string,
                  [<Optional>] quality    : Nullable<int>,
                  [<Optional>] optimize   : Nullable<bool>,
                  [<Optional>] weightX    : Nullable<int>,
                  [<Optional>] weightY    : Nullable<int>) =
  member private this.DisplayValue = this.ToString ()
  member val ImageType  = imageType
  member val Width      = width
  member val Height     = height
  member val ResizeMode = resizeMode
  member val Quality    = quality
  member val Optimize   = optimize
  member val WeightX    = weightX
  member val WeightY    = weightY
  override this.ToString () =
    let first = ref true
    StringBuilder()
    |> builderStart
    |> appends first "ImageType"  this.ImageType
    |> appendn first "Width"      this.Width
    |> appendn first "Height"     this.Height
    |> appends first "ResizeMode" this.ResizeMode
    |> appendn first "Quality"    this.Quality
    |> appendn first "Optimize"   this.Optimize
    |> appendn first "WeightX"    this.WeightX
    |> appendn first "WeightY"    this.WeightY
    |> builderEnd
    |> builderToString

[<NoEquality; NoComparison>]
type ImageInfo = {
  Width  : int
  Height : int
  XResolution : int
  YResolution : int
  Iptc : ImmutableDictionary<string, string>
  Exif : ImmutableDictionary<string, string> }

[<RequireQualifiedAccess>]
module ResizeModes =

  [<CompiledName("None")>]
  let none = "none"

  [<CompiledName("Exact")>]
  let exact = "exact"

  [<CompiledName("Inbox")>]
  let inbox = "inbox"