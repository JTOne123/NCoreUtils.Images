namespace NCoreUtils.Images

open System
open System.Runtime.Serialization

module private ImageResizerExceptionKeys =
  [<Literal>]
  let KeyImageResizeError = "ImageResizerError"
  [<Literal>]
  let KeyImageResizeErrorType = "ImageResizerErrorType"

[<Serializable>]
type ImageResizerError  =
  val mutable private message : string
  val mutable private description : string
  internal new (message, description) = { message = message; description = description }
  private new () = ImageResizerError (null, null)
  member this.Message = this.message
  member this.Description = this.description
  member private this.Message with set message = this.message <- message
  member private this.Description with set description = this.description <- description
  abstract RaiseException : unit -> 'a
  default this.RaiseException () = ImageResizerException this |> raise

and
  [<Serializable>]
  InvalidArgumentError =
    inherit ImageResizerError
    internal new (message, description) = { inherit ImageResizerError (message, description) }
    private new () = InvalidArgumentError (null, null)
    override this.RaiseException () = ImageResizerException this |> raise

and
  [<Serializable>]
  InvalidResizeModeError =
    inherit InvalidArgumentError
    val mutable private resizeMode : string
    internal new (message, description, resizeMode) =
      { inherit InvalidArgumentError (message, description)
        resizeMode = resizeMode }
    private new () = InvalidResizeModeError (null, null, null)
    member this.ResizeMode = this.resizeMode
    member private this.ResizeMode with set resizeMode = this.resizeMode <- resizeMode
    override this.RaiseException () = InvalidResizeModeException this |> raise

and
  [<Serializable>]
  InvalidImageTypeError =
    inherit InvalidArgumentError
    val mutable private imageType : string
    internal new (message, description, imageType) =
      { inherit InvalidArgumentError (message, description)
        imageType = imageType }
    private new () = InvalidImageTypeError (null, null, null)
    member this.ImageType = this.imageType
    member private this.ImageType with set imageType = this.imageType <- imageType
    override this.RaiseException () = InvalidImageTypeException this |> raise

and
  [<Serializable>]
  ImageResizerException =
    inherit Exception
    val private error : ImageResizerError
    new (error : ImageResizerError) =
      { inherit Exception (error.Message)
        error = error }
    new (error : ImageResizerError, innerException : exn) =
      { inherit Exception (error.Message, innerException)
        error = error }
    new (info : SerializationInfo, context) =
      let errorType = info.GetString ImageResizerExceptionKeys.KeyImageResizeErrorType |> Type.GetType
      { inherit Exception (info, context)
        error = info.GetValue (ImageResizerExceptionKeys.KeyImageResizeError, errorType) :?> ImageResizerError }
    member this.Error = this.error
    override this.Message = this.error.Message
    member this.Descripton = this.error.Description
    override this.GetObjectData (info, context) =
      base.GetObjectData (info, context)
      let errorType = this.error.GetType ()
      info.AddValue (ImageResizerExceptionKeys.KeyImageResizeErrorType, errorType.AssemblyQualifiedName)
      info.AddValue (ImageResizerExceptionKeys.KeyImageResizeError, this.error, errorType)


and
  [<Serializable>]
  InvalidResizeModeException =
    inherit ImageResizerException
    new (error : InvalidResizeModeError) = { inherit ImageResizerException (error) }
    new (error : InvalidResizeModeError, innerException : exn) = { inherit ImageResizerException (error, innerException) }
    new (info : SerializationInfo, context) = { inherit ImageResizerException (info, context) }
    member this.ResizeMode = (this.Error :?> InvalidResizeModeError).ResizeMode

and
  [<Serializable>]
  InvalidImageTypeException =
    inherit ImageResizerException
    new (error : InvalidImageTypeError) = { inherit ImageResizerException (error) }
    new (error : InvalidImageTypeError, innerException : exn) = { inherit ImageResizerException (error, innerException) }
    new (info : SerializationInfo, context) = { inherit ImageResizerException (info, context) }
    member this.ImageType = (this.Error :?> InvalidImageTypeError).ImageType

type ImageOptimizationError =
  inherit ImageResizerError
  val mutable private optimizer : string
  new (message, description, optimizer) =
    { inherit ImageResizerError (message, description)
      optimizer = optimizer }
  private new () = ImageOptimizationError (null, null, null)
  member this.Optimizer = this.optimizer
  member private this.Optimizer with set optimizer = this.optimizer <- optimizer
  override this.RaiseException () = ImageOptimizationException this |> raise

and
  [<Serializable>]
  ImageOptimizationException =
    inherit ImageResizerException
    new (error : ImageOptimizationError) = { inherit ImageResizerException (error) }
    new (error : ImageOptimizationError, innerException : exn) = { inherit ImageResizerException (error, innerException) }
    new (info : SerializationInfo, context) = { inherit ImageResizerException (info, context) }
    member this.Optimizer = (this.Error :?> ImageOptimizationError).Optimizer

[<CompiledName("ImageResizerErrors")>]
module ImageResizerError =

  [<CompiledName("Generic")>]
  let generic error errorDescription = new ImageResizerError (error, errorDescription)

  [<CompiledName("InvalidMode")>]
  let invalidMode resizeMode =
    InvalidResizeModeError (
      message     = sprintf "Invalid or unsupported resize mode = %s." resizeMode,
      description = "Specified mode is either invalid or not supported by the acutal instance of the image resizer.",
      resizeMode  = resizeMode)

  [<CompiledName("InvalidImageType")>]
  let invalidImageType imageType =
    InvalidImageTypeError (
      message     = sprintf "Invalid or unsupported image type = %s." imageType,
      description = "Specified image type is either invalid or not supported by the acutal instance of the image resizer.",
      imageType   = imageType)

  [<CompiledName("InvalidImage")>]
  let invalidImage =
    ImageResizerError (
      message     = "Invalid image data specified",
      description = "Specified image type is either invalid or not supported by the acutal instance of the image resizer.")

  [<CompiledName("OptimizationFailed")>]
  let optimizationFailed optimizer description =
    ImageOptimizationError (
      message     = "Optimization failed",
      description = description,
      optimizer   = optimizer)

  [<CompiledName("FormatOptimizationFailed")>]
  let optimizationFailedf optimizer fmt = Printf.kprintf (optimizationFailed optimizer) fmt
