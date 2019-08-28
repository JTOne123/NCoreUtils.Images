namespace NCoreUtils.Images

open System

type private SerializationInfo = System.Runtime.Serialization.SerializationInfo
type private SerializationException = System.Runtime.Serialization.SerializationException

// RemoteImageResizerError ---------------------------------------------------------------------------------------------

[<Serializable>]
type RemoteImageResizerError =
  inherit ImageResizerError

  val mutable private uri : string

  internal new (message, description, uri) =
    { inherit ImageResizerError (message, description)
      uri = uri }

  private new () =
    RemoteImageResizerError (null, null, null)

  member this.Uri = this.uri

  member private this.Uri
    with set uri = this.uri <- uri

  override this.RaiseException () =
    RemoteImageResizerException this |> raise

// RemoteImageResizerStatusCodeError -----------------------------------------------------------------------------------

and
  [<Serializable>]
  RemoteImageResizerStatusCodeError =
    inherit RemoteImageResizerError

    val mutable private statusCode : int

    internal new (message, description, uri, statusCode) =
      { inherit RemoteImageResizerError (message, description, uri)
        statusCode = statusCode }

    private new () =
      RemoteImageResizerStatusCodeError (null, null, null, 0)

    member this.StatusCode = this.statusCode

    member private this.StatusCode
      with set statusCode = this.statusCode <- statusCode

    override this.RaiseException () =
      RemoteImageResizerStatusCodeException this |> raise

// RemoteImageResizerClrError ------------------------------------------------------------------------------------------

and
  [<Serializable>]
  RemoteImageResizerClrError =
    inherit RemoteImageResizerError

    val mutable private innerException : exn

    internal new (message, description, uri, innerException) =
      { inherit RemoteImageResizerError (message, description, uri)
        innerException = innerException }

    private new () =
      RemoteImageResizerClrError (null, null, null, null)

    member this.InnerException = this.innerException

    member private this.InnerException
      with set innerException = this.innerException <- innerException

    override this.RaiseException () =
      RemoteImageResizerException (this, this.InnerException) |> raise

// RemoteImageResizerException -----------------------------------------------------------------------------------------

and
  [<Serializable>]
  RemoteImageResizerException =
    inherit ImageResizerException

    new (error : RemoteImageResizerError) =
      { inherit ImageResizerException (error) }

    new (error : RemoteImageResizerError, innerException) =
      { inherit ImageResizerException (error, innerException) }

    new (info : SerializationInfo, context) =
      { inherit ImageResizerException (info, context) }

    member this.RequestUri =
      (this.Error :?> RemoteImageResizerError).Uri

// RemoteImageResizerStatusCodeException -------------------------------------------------------------------------------

and
  [<Serializable>]
  RemoteImageResizerStatusCodeException =
    inherit RemoteImageResizerException

    new (error : RemoteImageResizerStatusCodeError) =
      { inherit RemoteImageResizerException (error) }

    new (error : RemoteImageResizerStatusCodeError, innerException) =
      { inherit RemoteImageResizerException (error, innerException) }

    new (info : SerializationInfo, context) =
      { inherit RemoteImageResizerException (info, context) }

    member this.StatusCode =
      (this.Error :?> RemoteImageResizerStatusCodeError).StatusCode
