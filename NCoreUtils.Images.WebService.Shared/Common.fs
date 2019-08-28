namespace NCoreUtils.Images

[<RequireQualifiedAccess>]
module Routes =

  [<Literal>]
  let Capabilities = "capabilities"

  [<Literal>]
  let Info = "info"

/// Defines predefined capabilities of the remote image resizing service.
[<RequireQualifiedAccess>]
module Capabilities =

  [<CompiledName("SerializedImageInfo")>]
  let serializedImageInfo = "https://ncoreutils.io/images/serialized"