using System.Data;

namespace ExerciseReport
{
    internal class PathNames
    {
        private const string ProductionRoot = ".";
        private const string TestUserRoot = "/Users/mikedamay/projects/exercism/v3";
        
        public string Root { get; }
        public string Languages { get; } = "languages";
        public string TrackNeutralConcepts { get; } = "reference/concepts";
        public string Exercises { get; } = "exercises/concept";
        public string DesignDocName { get; } = ".meta/design.md";
        public string ExerciseFile { get; } = "reference/exercises.json";
        public string ErrorsFile { get; } = "reference/exercise-errors.json";
        public string ConceptReport { get; } = "reference/README.md";

        public static PathNames Default { get; } =
            new PathNames(ProductionRoot);

        public static PathNames Test { get; } =
            new PathNames(TestUserRoot);

        private PathNames(string root)
        {
            Root = root;
        }
    }
}