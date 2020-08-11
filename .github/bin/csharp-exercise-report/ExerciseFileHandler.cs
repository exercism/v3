using System.IO;

namespace ExerciseReport
{
    public interface IExerciseFileHandler
    {
        string ReadFile();
    }

    internal class ExerciseFileHandler : IExerciseFileHandler
    {
        private readonly string exercisePathAndFileName;

        public ExerciseFileHandler(string root, string track)
        {
            exercisePathAndFileName = Path.Combine(
                root,
                PathNames.Default.Languages,
                track,
                PathNames.Default.ExerciseFile);
        }

        public string ReadFile()
        {
            return File.ReadAllText(exercisePathAndFileName);
        }

        public void WriteFile(string exerciseJson)
        {
            File.WriteAllText(exercisePathAndFileName, exerciseJson);
        }

    }
}