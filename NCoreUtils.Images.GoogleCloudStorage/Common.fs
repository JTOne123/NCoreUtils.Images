namespace NCoreUtils.Images

open System
open System.Collections.Immutable
open System.Collections.Generic
open System.Runtime.InteropServices
open System.Text
open Google.Apis.Auth.OAuth2
open Google.Apis.Storage.v1
open NCoreUtils
open NCoreUtils.IO
open System.Net.Http
open System.Runtime.Serialization
open System.Net

[<Serializable>]
type GoogleCloudStorageAccessException =
  inherit Exception

  new (message) = { inherit Exception (message) }

  new (message, innerExn : exn) = { inherit Exception (message, innerExn) }

  new (info : SerializationInfo, context) = { inherit Exception (info, context) }

[<AutoOpen>]
module private GoogleCloudStorageHelpers =

  let toProducer generator =
    { new IStreamProducer with
        member __.AsyncProduce output = generator output
        member __.Dispose () = ()
    }

type GoogleStorageCredential =
  | ViaAccessToken      of AccessToken : string
  | ViaGoogleCredential of Credential : GoogleCredential
  | ViaDefault
  with
    static member FromCredential credential =
      match credential with
      | null -> ViaDefault
      | _    -> ViaGoogleCredential credential
    static member op_Implicit (accessToken : string) = ViaAccessToken accessToken
    static member op_Implicit (googleCredential : GoogleCredential) =
      GoogleStorageCredential.FromCredential googleCredential

type GoogleCloudStorageRecordDescriptor =
  val public Uri          : Uri
  val public Credential   : GoogleStorageCredential
  val public ContentType  : string
  val public CacheControl : string
  val public IsPublic     : bool
  new (uri,
        credential,
        [<Optional; DefaultParameterValue(null : string)>] contentType,
        [<Optional; DefaultParameterValue(null : string)>] cacheControl,
        [<Optional; DefaultParameterValue(false)>] isPublic) =
    if isNull uri then nullArg "uri"
    { Uri          = uri
      Credential   = credential
      ContentType  = contentType
      CacheControl = cacheControl
      IsPublic     = isPublic }


[<RequireQualifiedAccess>]
module GoogleStorageCredential =

  [<CompiledName("ReadOnlyScopes")>]
  let readOnlyScopes = [| "https://www.googleapis.com/auth/devstorage.read_only" |]

  [<CompiledName("ReadWriteScopes")>]
  let readWriteScopes = [| "https://www.googleapis.com/auth/devstorage.read_write" |]

  [<CompiledName("AsyncGetAccessToken")>]
  let asyncGetAccessToken scopes cred  =
    match cred with
    | ViaAccessToken accessToken -> async.Return accessToken
    | ViaGoogleCredential credential ->
      let c = credential.CreateScoped scopes
      Async.Adapt (fun cancellationToken -> c.UnderlyingCredential.GetAccessTokenForRequestAsync (cancellationToken = cancellationToken))
    | ViaDefault ->
      async {
        let! credential = Async.Adapt (fun _ -> GoogleCredential.GetApplicationDefaultAsync ())
        let  c = credential.CreateScoped scopes
        return! Async.Adapt (fun cancellationToken -> c.UnderlyingCredential.GetAccessTokenForRequestAsync (cancellationToken = cancellationToken)) }


type GoogleStorageRecordProducer =
  val public Uri        : Uri

  val public Credential : GoogleStorageCredential

  new (uri, credential : GoogleStorageCredential) =
    if isNull uri then nullArg "uri"
    { Uri = uri
      Credential = credential }

  new (uri, credential : GoogleCredential) =
    let cred = GoogleStorageCredential.FromCredential credential
    new GoogleStorageRecordProducer (uri, cred)

  new (uri, accessToken : string) =
    new GoogleStorageRecordProducer (uri, ViaAccessToken accessToken)

  new (uri) =
    new GoogleStorageRecordProducer (uri, ViaDefault)

  interface ISerializableImageProducer with
    member this.AsyncProduce output = async {
      let! token      = GoogleStorageCredential.asyncGetAccessToken GoogleStorageCredential.readOnlyScopes this.Credential
      // TODO: use factory
      use  client     = new HttpClient ()
      let  requestUri = sprintf "https://www.googleapis.com/storage/v1/b/%s/o/%s?alt=media" this.Uri.Host (this.Uri.LocalPath.Trim '/' |> Uri.EscapeDataString )
      use  request    = new HttpRequestMessage (HttpMethod.Get, requestUri)
      request.Headers.Authorization <- Headers.AuthenticationHeaderValue ("Bearer", token)
      use! response = client.AsyncSend (request, HttpCompletionOption.ResponseHeadersRead)
      do!
        Async.Adapt
          (fun _ ->
            match response.StatusCode with
            | HttpStatusCode.OK ->
              response.Content.CopyToAsync output
            | statusCode ->
              sprintf "Objects.get failed, status code = %A, uri = %s" statusCode requestUri
              |> GoogleCloudStorageAccessException
              |> raise)
      output.Close () }

    member this.AsyncSerialize () = async {
      let! token = GoogleStorageCredential.asyncGetAccessToken GoogleStorageCredential.readOnlyScopes this.Credential
      return
        { Body = Encoding.ASCII.GetBytes this.Uri.AbsoluteUri
          Info = ImmutableDictionary.CreateRange (
                  StringComparer.OrdinalIgnoreCase,
                  [|  KeyValuePair ("SourceType", "GoogleCloudStorageRecord")
                      KeyValuePair ("SourceAccessToken", token) |] ) } }

    member __.Dispose(): unit = ()

type GoogleStorageRecordDestination =
  inherit GoogleCloudStorageRecordDescriptor

  new (uri,
        credential : GoogleStorageCredential,
        [<Optional; DefaultParameterValue(null : string)>] contentType,
        [<Optional; DefaultParameterValue(null : string)>] cacheControl,
        [<Optional; DefaultParameterValue(false)>] isPublic) =
    { inherit GoogleCloudStorageRecordDescriptor(uri, credential, contentType, cacheControl, isPublic) }

  new (descriptor : GoogleCloudStorageRecordDescriptor) =
    let d = descriptor
    { inherit GoogleCloudStorageRecordDescriptor(d.Uri, d.Credential, d.ContentType, d.CacheControl, d.IsPublic) }

  new (uri : Uri) =
    new GoogleStorageRecordDestination (uri, ViaDefault)

  member private this.AsyncUpload (input, obj : Data.Object) = async {
    use service = new StorageService ()
    let request = service.Objects.Insert (obj, this.Uri.Host, input, obj.ContentType |?? "application/octet-stream")
    let! token = GoogleStorageCredential.asyncGetAccessToken GoogleStorageCredential.readWriteScopes this.Credential
    request.OauthToken <- token
    let! _ = Async.Adapt (fun cancellationToken -> request.UploadAsync cancellationToken)
    () }

  interface IImageDestination with
    member this.AsyncWrite (contentInfo, generator) =
      let obj =
        let ci = contentInfo |?? ContentInfo.Empty
        let contentType =
          match this.ContentType with
          | null | ""   -> ci.ContentType |?? "application/octet-stream"
          | contentType -> contentType
        let acl =
          let acl = ResizeArray ()
          if this.IsPublic then
            acl.Add <| Data.ObjectAccessControl (Entity = "allUsers", Role = "READER")
          acl
        Data.Object (
          Name         = (this.Uri.LocalPath.Trim '/'),
          Size         = (ci.ContentLength >>|  uint64),
          CacheControl = this.CacheControl,
          Acl          = acl,
          ContentType  = contentType)
      let consumer =
        { new IStreamConsumer with
            member __.AsyncConsume input = this.AsyncUpload (input, obj)
            member __.Dispose () = ()
        }
      StreamConsumer.asyncConsumeProducer (toProducer generator) consumer

    member this.AsyncWriteDelayed generator =
      let acl =
        let acl = ResizeArray ()
        if this.IsPublic then
          acl.Add <| Data.ObjectAccessControl (Entity = "allUsers", Role = "READER")
        acl
      let obj =
        Data.Object (
          Name         = (this.Uri.LocalPath.Trim '/'),
          CacheControl = this.CacheControl,
          Acl          = acl,
          ContentType  = this.ContentType)
      let setContentInfo contentInfo =
        let ci = contentInfo |?? ContentInfo.Empty
        obj.Size        <- ci.ContentLength >>|  uint64
        if not (String.IsNullOrEmpty ci.ContentType) then
          obj.ContentType <- ci.ContentType
      let consumer =
        { new IStreamConsumer with
            member __.AsyncConsume input = this.AsyncUpload (input, obj)
            member __.Dispose () = ()
        }
      StreamConsumer.asyncConsumeProducer (generator setContentInfo |> toProducer) consumer
  interface ISerializableImageDestination with
    member this.AsyncSerialize () = async {
      let! token = GoogleStorageCredential.asyncGetAccessToken GoogleStorageCredential.readWriteScopes this.Credential
      let  info  =
        let builder = ImmutableDictionary.CreateBuilder StringComparer.OrdinalIgnoreCase
        builder.Add ("DestinationType", "GoogleCloudStorageRecord")
        builder.Add ("DestinationUri", this.Uri.AbsoluteUri)
        builder.Add ("DestinationAccessToken", token)
        if not (String.IsNullOrEmpty this.ContentType) then
          builder.Add ("DestinationContentType", this.ContentType)
        if not (String.IsNullOrEmpty this.CacheControl) then
          builder.Add ("DestinationCacheControl", this.CacheControl)
        if this.IsPublic then
          builder.Add ("DestinationIsPublic", if this.IsPublic then "true" else "false")
        builder.ToImmutable ()
      return info :> _ }


