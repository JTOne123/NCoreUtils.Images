namespace NCoreUtils.Images.Unit
{
    public static class Resources
    {
        static byte[] ReadResource(string name)
        {
            var assembly = typeof(Resources).Assembly;
            var resourceStream = assembly.GetManifestResourceStream($"NCoreUtils.Images.Unit.{name}");
            var result = new byte[resourceStream.Length];
            resourceStream.Read(result, 0, (int)resourceStream.Length);
            return result;
        }

        public static class Png
        {
            public static byte[] X { get; } = ReadResource("Resources.x.png");
        }
    }
}