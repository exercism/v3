using System.IO;

namespace ExerciseReport
{
    public interface IExerciseFileHandler
    {
        string ReadExerciseFile();
        
        void WriteExerciseFile(string exerciseJson);

        string ReadErrorFile();
        
        void WriteErrorFile(string errorJson);
    }

    internal class ExerciseFileHandler : IExerciseFileHandler
    {
        private readonly string exercisePathAndFileName;
        private readonly string errorPathAndFileName;

        public ExerciseFileHandler(string root, string track)
        {
            exercisePathAndFileName = Path.Combine(
                root,
                PathNames.Default.Languages,
                track,
                PathNames.Default.ExerciseFile);
            errorPathAndFileName = Path.Combine(
                root,
                PathNames.Default.Languages,
                track,
                PathNames.Default.ErrorsFile);
        }

        public string ReadExerciseFile()
        {
            return File.ReadAllText(exercisePathAndFileName);
        }

        public void WriteExerciseFile(string exerciseJson)
        {
            File.WriteAllText(exercisePathAndFileName, exerciseJson);
        }

        public string ReadErrorFile()
        {
            return File.ReadAllText(errorPathAndFileName);
        }

        public void WriteErrorFile(string errorJson)
        {
            File.WriteAllText(errorPathAndFileName, errorJson);
        }
    }
}