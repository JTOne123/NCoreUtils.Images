namespace NCoreUtils.Images

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.DependencyInjection.Extensions
open Microsoft.Extensions.Logging
open NCoreUtils.Images.Internal
open System.Net.Http

[<Sealed>]
type internal LoggerAdapter<'T> (logger : ILogger<'T>) =
  interface ILog<'T> with
    member __.LogDebug   (exn, message) = logger.LogDebug (exn, message)
    member __.LogWarning (exn, message) = logger.LogWarning (exn, message)
    member __.LogError   (exn, message) = logger.LogError (exn, message)

[<AutoOpen>]
module private Helpers =

  let inline toResizeArray (seq : seq<_>) = ResizeArray seq

[<Extension>]
[<Sealed; AbstractClass>]
type ServiceCollectionNCoreUtilsImagesExtensions =

  [<Extension>]
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  static member AddImageResizer<'TResizer when 'TResizer : not struct and 'TResizer :> IImageResizer>(services : IServiceCollection, [<Optional; DefaultParameterValue(ServiceLifetime.Transient)>] serviceLifetime : ServiceLifetime) =
    services
      .AddTransient(typedefof<ILog<_>>, typedefof<LoggerAdapter<_>>)
      .Add(ServiceDescriptor (typeof<IImageResizer>, typeof<'TResizer>, serviceLifetime))
    services

  [<Extension>]
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  static member AddImageOptimization<'TOptimization when 'TOptimization : not struct and 'TOptimization :> IImageOptimization>(services : IServiceCollection, [<Optional; DefaultParameterValue(ServiceLifetime.Singleton)>] serviceLifetime : ServiceLifetime) =
    ServiceDescriptor (typeof<IImageOptimization>, typeof<'TOptimization>, serviceLifetime) |> services.TryAddEnumerable
    services

  [<Extension>]
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  static member AddSingletonImageOptimization<'TOptimization when 'TOptimization : not struct and 'TOptimization :> IImageOptimization>(services : IServiceCollection) =
    services.AddImageOptimization<'TOptimization> ServiceLifetime.Singleton

  [<Extension>]
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  static member AddScopedImageOptimization<'TOptimization when 'TOptimization : not struct and 'TOptimization :> IImageOptimization>(services : IServiceCollection) =
    services.AddImageOptimization<'TOptimization> ServiceLifetime.Scoped

  [<Extension>]
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  static member AddTransientImageOptimization<'TOptimization when 'TOptimization : not struct and 'TOptimization :> IImageOptimization>(services : IServiceCollection) =
    services.AddImageOptimization<'TOptimization> ServiceLifetime.Transient

[<Extension>]
[<Sealed; AbstractClass>]
type ServiceProviderNCoreUtilsImagesExtensions =

  [<Extension>]
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  static member GetImageOptimizations (serviceProvider : IServiceProvider) =
    match serviceProvider.GetServices<IImageOptimization> () with
    | null -> Seq.empty
    | seq  -> seq

  [<Extension>]
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  static member TryGetImageOptimizations (serviceProvider : IServiceProvider, imageType : string, [<Out>] optimizations : byref<seq<IImageOptimization>>) =
    let inline supports (optimization : IImageOptimization) = optimization.Supports imageType
    let opts =
      serviceProvider.GetImageOptimizations ()
      |> Seq.filter supports
      |> toResizeArray
    match opts.Count with
    | 0 ->
      optimizations <- Unchecked.defaultof<_>
      false
    | _ ->
      optimizations <- opts
      true

  [<Extension>]
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  static member TryGetImageOptimizations (serviceProvider : IServiceProvider, imageType : ImageType, [<Out>] optimizations : byref<seq<IImageOptimization>>) =
    serviceProvider.TryGetImageOptimizations (ImageType.toExtension imageType, &optimizations)


[<AutoOpen>]
module ServiceProviderExt =

  type IServiceProvider with
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.TryGetImageOptimizations (imageType : string) =
      let mutable optimizations = Unchecked.defaultof<_>
      match this.TryGetImageOptimizations (imageType, &optimizations) with
      | true -> Some optimizations
      | _    -> None

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.TryGetImageOptimizations (imageType : ImageType) =
      let mutable optimizations = Unchecked.defaultof<_>
      match this.TryGetImageOptimizations (imageType, &optimizations) with
      | true -> Some optimizations
      | _    -> None