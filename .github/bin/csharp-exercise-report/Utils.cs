using System;
using System.IO;

namespace ExerciseReport
{
    public static class Utils
    {
        public static string GetResourceAsString(string resourceName)
        {
            string resourcePath = $"ExerciseReport.Data.{resourceName}";
            string resourceText = String.Empty;
            Stream? stream = typeof(Utils).Assembly.GetManifestResourceStream(resourcePath);
            if (stream != null)
            {
                using (stream)
                using (var reader = new StreamReader(stream))
                    resourceText = reader.ReadToEnd();
                return resourceText;
            }
            else
            {
                throw new NullReferenceException($"{nameof(stream)} is null - missing resource {resourcePath}");
            }
        }
    }
}