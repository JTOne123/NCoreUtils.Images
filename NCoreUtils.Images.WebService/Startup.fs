namespace NCoreUtils.Images.WebService

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open NCoreUtils.AspNetCore
open NCoreUtils.Images
open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open System
open System.Threading
// open NCoreUtils.Images.Optimization

type Startup (env : IWebHostEnvironment) =

  static let configureSourceExtractors =
    Action<CompositeImageSourceExtractorBuilder>
      (fun builder -> builder.AddExtractor<GoogleCloudStorageSourceExtractor> () |> ignore)

  static let configureDestinationExtractors =
    Action<CompositeImageDestinationExtractorBuilder>
      (fun builder -> builder.AddExtractor<GoogleCloudStorageDestinationExtractor> () |> ignore)

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
          .AddConsole () |> ignore
      )
      .AddSingleton(JsonSerializerSettings (ReferenceLoopHandling = ReferenceLoopHandling.Ignore, ContractResolver = CamelCasePropertyNamesContractResolver ()))
      .AddSingleton<IHttpContextAccessor, HttpContextAccessor>()
      // .AddSingletonImageOptimization<JpegoptimOptimization>()
      .AddImageSourceExtractors(configureSourceExtractors)
      .AddImageDestinationExtractors(configureDestinationExtractors)
      .AddImageMagickResizer()
      |> ignore

  member __.Configure (app: IApplicationBuilder, serviceConfiguration : ServiceConfiguration, loggerFactory : ILoggerFactory) =
    let semaphore = new SemaphoreSlim (serviceConfiguration.MaxConcurrentOps, serviceConfiguration.MaxConcurrentOps)

    if "Development" = env.EnvironmentName then
      let logger = loggerFactory.CreateLogger typeof<ImageMagick.MagickNET>
      ImageMagick.MagickNET.SetLogEvents (ImageMagick.LogEvents.Image ||| ImageMagick.LogEvents.Coder ||| ImageMagick.LogEvents.Transform)
      ImageMagick.MagickNET.Log.AddHandler (fun _ e -> logger.LogInformation (sprintf "[%s] %s" (e.EventType.ToString ()) e.Message))

    app
      .Use(GCMiddleware.run)
      .Use(Middleware.run semaphore) |> ignore
