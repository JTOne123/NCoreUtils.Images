using System;
using System.Collections.Immutable;
using System.Threading;
using System.Threading.Tasks;
using NCoreUtils.IO;

namespace NCoreUtils.Images.Unit
{
    public class DummyResizer : AsyncImageResizer
    {
        public override Task<ImageInfo> GetImageInfoAsync(IStreamProducer source, CancellationToken cancellationToken)
        {
            return Task.FromResult(new ImageInfo(100, 100, 96, 96, ImmutableDictionary<string, string>.Empty, ImmutableDictionary<string, string>.Empty));
        }

        public override Task ResizeAsync(IStreamProducer source, IImageDestination destination, ResizeOptions options, CancellationToken cancellationToken)
        {
            return destination.WriteDelayedAsync((setContentInfo, stream, ctoken) =>
            {
                setContentInfo(new ContentInfo (null, null));
                return source.ProduceAsync(stream, ctoken);
            }, cancellationToken);
        }

        public override async Task<AsyncResult<ImageResizerError>> TryResizeAsync(IStreamProducer source, IImageDestination destination, ResizeOptions options, CancellationToken cancellationToken)
        {
            try
            {
                await this.ResizeAsync(source, destination, options, cancellationToken);
                return AsyncResult.Success<ImageResizerError>();
            }
            catch (ImageResizerException exn)
            {
                return AsyncResult.Error(exn.Error);
            }
            catch (Exception exn)
            {
                return AsyncResult.Error(ImageResizerErrorModule.Generic("Image resize failed.", exn.Message));
            }
        }
    }
}