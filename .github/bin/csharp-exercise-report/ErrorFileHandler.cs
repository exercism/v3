using System.IO;

namespace ExerciseReport
{
    public interface IErrorFileHandler
    {

        string ReadFile();
        
        void WriteFile(string errorJson);
    }

    internal class ErrorFileHandler : IErrorFileHandler
    {
        private readonly string pathAndFileName;

        public ErrorFileHandler(string root, string track)
        {
            pathAndFileName = Path.Combine(
                root,
                PathNames.Default.Languages,
                track,
                PathNames.Default.ErrorsFile);
        }

        public string ReadFile()
        {
            return File.ReadAllText(pathAndFileName);
        }

        public void WriteFile(string errorJson)
        {
            File.WriteAllText(pathAndFileName, errorJson);
        }
    }
}