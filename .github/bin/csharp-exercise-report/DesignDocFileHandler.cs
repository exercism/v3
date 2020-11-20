using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace ExerciseReport
{
    internal interface IDesignDocFileHandler
    {
        IEnumerable<(string DesignDocPath, string DesignDocContents)> GetExerciseDesignsForTrack();
    }
    internal class DesignDocFileHandler : IDesignDocFileHandler
    {
        private readonly string root;
        private readonly string track;

        public DesignDocFileHandler(string root, string track)
        {
            this.root = root;
            this.track = track;
        }

        public IEnumerable<(string DesignDocPath, string DesignDocContents)> GetExerciseDesignsForTrack()
        {
            var exercisePaths = Directory.EnumerateDirectories(Path.Combine(
                root,
                PathNames.Default.Languages,
                track,
                PathNames.Default.Exercises
                ));    // ./languages/csharp/exercises/concept
            var designDetails = exercisePaths
                .Select(exp => Path.Combine(exp, PathNames.Default.DesignDocName))
                .Where(path => File.Exists(path))
                .Select(path => (path, text: File.ReadAllText(path)));
            return designDetails;
        }
    }
}