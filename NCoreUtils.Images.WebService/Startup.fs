namespace NCoreUtils.Images.WebService

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open NCoreUtils.AspNetCore
open NCoreUtils.Logging
open NCoreUtils.Images
open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open System.Threading
open NCoreUtils.Images.Optimization

type Startup (env : IHostingEnvironment) =

  member __.ConfigureServices (services: IServiceCollection) =
    let configuration = Config.buildConfig ()
    let imageResizerOptions = configuration.GetSection("Images").Get<ImageResizerOptions>()
    let serviceConfiguration = configuration.GetSection("Images").Get<ServiceConfiguration>()
    if box imageResizerOptions |> isNull |> not
      then
        services.AddSingleton<IImageResizerOptions> imageResizerOptions |> ignore
        services.AddSingleton<ImageResizerOptions> imageResizerOptions |> ignore
      else
        services.AddSingleton<IImageResizerOptions> ImageResizerOptions.Default |> ignore
        services.AddSingleton<ImageResizerOptions>  ImageResizerOptions.Default |> ignore
    if box serviceConfiguration |> isNull |> not
      then
        services.AddSingleton<ServiceConfiguration> serviceConfiguration |> ignore
      else
        services.AddSingleton<ServiceConfiguration> (ServiceConfiguration (MaxConcurrentOps = 20)) |> ignore
    services
      .AddSingleton<IConfiguration>(configuration)
      .AddLogging(fun b ->
        b.ClearProviders()
          .SetMinimumLevel(LogLevel.Information)
          |> ignore
        match env.IsDevelopment () with
        | true -> b.AddConsole () |> ignore
        | _    -> b.AddGoogleSink (configuration.GetSection "Google") |> ignore
      )
      .AddPrePopulatedLoggingContext()
      .AddSingleton(JsonSerializerSettings (ReferenceLoopHandling = ReferenceLoopHandling.Ignore, ContractResolver = CamelCasePropertyNamesContractResolver ()))
      .AddSingleton<IHttpContextAccessor, HttpContextAccessor>()
      .AddSingletonImageOptimization<JpegoptimOptimization>()
      .AddImageMagickResizer()
      // .AddCoreImageResizer<NCoreUtils.Images.ImageSharp.ImageProvider>(ServiceLifetime.Singleton)
      |> ignore

  member __.Configure (app: IApplicationBuilder, serviceConfiguration : ServiceConfiguration) =
    let semaphore = new SemaphoreSlim (serviceConfiguration.MaxConcurrentOps, serviceConfiguration.MaxConcurrentOps)

    // ImageMagick.MagickNET.SetLogEvents ImageMagick.LogEvents.None
    // ImageMagick.MagickNET.Log.AddHandler (fun _ e -> printfn "%s" e.Message)

    app
      .UsePrePopulateLoggingContext()
      .Use(GCMiddleware.run)
      .Use(Middleware.run semaphore) |> ignore