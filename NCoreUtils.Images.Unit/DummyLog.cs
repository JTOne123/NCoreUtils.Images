using System;

namespace NCoreUtils.Images.Unit
{
    public class DummyLog<T> : ILog<T>
    {
        public void LogDebug(Exception exn, string message) { }

        public void LogError(Exception exn, string message) { }

        public void LogWarning(Exception exn, string message) { }
    }
}