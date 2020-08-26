using System.Data;

namespace ExerciseReport
{
    internal class PathNames
    {
        // private const string ProductionRoot = ".";
        private const string ProductionRoot = ".";
        
        public string Root { get; }
        public string Languages { get; } = "languages";
        public string TrackNeutralConcepts { get; } = "reference/concepts";
        public string Exercises { get; } = "exercises/concept";
        public string DesignDocName { get; } = ".meta/" + Constants.DesignMd;
        public string ExerciseFile { get; } = "reference/" + Constants.ExercisesJson;
        public string ErrorsFile { get; } = "reference/" + Constants.ExerciseErrorsJson;
        public string ConceptReport { get; } = "reference/README.md";

        public static PathNames Default { get; } =
            new PathNames(ProductionRoot);

        private PathNames(string root)
        {
            Root = root;
        }
    }
}