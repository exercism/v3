using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace ExerciseReport
{
    internal class DesignDocReader
    {
        private readonly DesignDocParser designDocParser;
        private readonly IDesignDocFileHandler designDocFileHandler;

        public DesignDocReader(IDesignDocFileHandler designDocFileHandler, DesignDocParser designDocParser)
        {
            this.designDocParser = designDocParser;
            this.designDocFileHandler = designDocFileHandler;
        }

        public (LearningObjectives learningObjectives, List<Error> errors) 
            GetAllLearningObjectives()
        {
            var errors = new List<Error>();
            var learningObjectivesBuilder = LearningObjectives.CreateBuilder();
            var conceptsAndObjectives = designDocFileHandler.GetExerciseDesignsForTrack()
                .SelectMany(
                    designDetails => designDocParser.ParseDesignDoc(
                        GetExerciseNameFromPath(designDetails.DesignDocPath),
                        designDetails.DesignDocContents));
            foreach (var conceptAndObjective in conceptsAndObjectives)
            {
                switch (conceptAndObjective)
                {
                    case (Result.Success, _, (string designDocId, string conceptName) conceptDetails, string objective):
                        learningObjectivesBuilder.Add(conceptDetails, objective);
                        break;
                    case (Result.Errors, string error, _, _):
                        errors.Add(new Error(ErrorSource.Design, Severity.Error, error));
                        break;
                    case (_, _, _, _):
                        throw new ArgumentException();
                }
            }

            return (learningObjectivesBuilder.CreateLearningObjectives(), errors);
        }
        // designDocPath: typically "./languages/<language>/exercises/concept/<exercise-name>/.meta/design.md"
        // returns: <exercise-name>
        private static string GetExerciseNameFromPath(string designDocPath)
        {
            var path = Path.GetDirectoryName(designDocPath);
            var parts = path?.Split("/") ?? new string[0];
            return parts.Length > 2 ? parts[^2] : designDocPath;
        }

    }
}