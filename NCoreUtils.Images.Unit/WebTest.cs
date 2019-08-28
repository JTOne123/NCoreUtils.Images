using System.IO;
using System.Linq;
using System.Net.Http;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.DependencyInjection.Extensions;
using NCoreUtils.Images.WebService;
using NCoreUtils.IO;
using Xunit;

namespace NCoreUtils.Images.Unit
{
    public class WebTest : WebTestBase<Startup>
    {
        class HttpClientFactory : IHttpClientFactoryAdapter
        {
            readonly WebTest _self;

            public HttpClientFactory(WebTest self)
            {
                _self = self;
            }

            public HttpClient CreateClient(string name)
            {
                return _self.CreateClient();
            }
        }

        public WebTest(WebApplicationFactory<Startup> factory) : base(factory) { }

        ImageResizerClient CreateResizerClient()
        {
            return new ImageResizerClient(
                new ImageResizerClientConfiguration
                {
                    EndPoints = new [] { "http://localhost/xxx", "http://localhost" }
                },
                new DummyLog<ImageResizerClient>(),
                new HttpClientFactory(this)
            );
        }

        protected override void InitializeServices(IServiceCollection services)
        {
            services
                .RemoveAll(typeof(IImageOptimization))
                .RemoveAll(typeof(IImageResizer))
                .AddSingleton<IImageResizer, DummyResizer>();
        }

        [Fact]
        public async Task SuccessfullResize()
        {
            var binary = Resources.Png.X;
            var client = CreateResizerClient();

            var info = await client.GetImageInfoAsync(StreamProducerModule.FromStream(new MemoryStream(binary, false)), CancellationToken.None);
            Assert.Equal (100, info.Width);

            using (var buffer = new MemoryStream())
            {
                await client.ResizeAsync(new MemoryStream(binary, false), buffer, new ResizeOptions(imageType: "png"), CancellationToken.None);
                Assert.True (Enumerable.SequenceEqual(binary, buffer.ToArray ()));
            }
        }
    }
}