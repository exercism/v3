using System;
using System.Collections.Generic;
using System.Linq;

namespace ExerciseReport
{
    internal class ExerciseMerger
    {
        private readonly ExerciseReader exerciseReader;
        private readonly DesignDocReader designDocReader;
        private readonly int maxErrors;

        public ExerciseMerger(ExerciseReader exerciseReader,
            DesignDocReader designDocReader,
            int maxErrors = Constants.MaxMissingLearningObjectives)
        {
            this.exerciseReader = exerciseReader;
            this.designDocReader = designDocReader;
            this.maxErrors = maxErrors;
        }

        public static ExerciseMerger CSharpMerger { get; } =
            new ExerciseMerger(
                new ExerciseReader(
                    new ExerciseFileHandler(PathNames.Default.Root, Constants.CSharpTrack),
                    new ExerciseJsonParser()),
                new DesignDocReader(
                    new DesignDocFileHandler(PathNames.Default.Root, Constants.CSharpTrack),
                    new DesignDocParser())
            );

        public (Result Result, ExerciseObjectTree ExerciseObjectTree, List<Error> Errors)
            MergeExercisesAndLearningObjectives()
        {
            var readResults = exerciseReader.ReadExercises();
            if (readResults.Result == Result.FatalError)
            {
                return readResults;
            }

            var learningObjectives = designDocReader.GetAllLearningObjectives();
            MergeLearningObjectives(readResults.ExerciseObjectTree, learningObjectives.learningObjectives);
            var unmatchedConcepts =
                ReportUnmatchedConcepts(readResults.ExerciseObjectTree, learningObjectives.learningObjectives);
            var missingLearningObjectives = ReportMissingLearningObjectives(readResults.ExerciseObjectTree);
            var combinedErrors = readResults.Errors
                .Concat(learningObjectives.errors)
                .Concat(unmatchedConcepts)
                .Concat(missingLearningObjectives)
                .ToList();
            var maxSeverity = combinedErrors.Select(e => e.Severity).DefaultIfEmpty(Severity.None).Max();
            Result result = SeverityToResult(maxSeverity);
            return (result, readResults.ExerciseObjectTree, combinedErrors);
        }

        private List<Error> ReportMissingLearningObjectives(ExerciseObjectTree exerciseObjectTree)
        {
            var errors = exerciseObjectTree.Exercises.Where(ex => ex.CompletionStatus == CompletionStatus.Complete)
                .SelectMany(ex => ex.Concepts, (Exercise, Concept) => (Exercise, Concept))
                .Where(e_and_c => e_and_c.Concept.LearningObjectives.Count == 0)
                .Select(e_and_c =>
                    new Error(ErrorSource.MissingLearningObjective,
                        Severity.Error,
                        $"The {e_and_c.Concept.Name} concept has no learning objectives on the exercise report - update the {Constants.DesignMd} for the {e_and_c.Exercise.Slug} exercise")
                ).ToList();
            if (errors.Count > maxErrors)
            {
                errors.Add(
                    new Error(ErrorSource.MissingLearningObjective,
                        Severity.Fatal,
                        $"Too many concepts have no learning objectives - see {Constants.ExerciseErrorsJson}")
                );
            }

            return errors;
        }

        private List<Error> ReportUnmatchedConcepts(ExerciseObjectTree exerciseObjectTree,
            LearningObjectives learningObjectives)
        {
            var exerciseMap = exerciseObjectTree.Exercises
                .SelectMany(ex => ex.Concepts)
                .Select(con => con.Name)
                .ToHashSet();

            List<Error> errors = new List<Error>();
            foreach ((string DocId, string ConceptName) conceptDetails in learningObjectives.ConceptsInclDesignDocId)
            {
                if (!exerciseMap.Contains(conceptDetails.ConceptName))
                {
                    errors.Add(new Error(ErrorSource.Merge, Severity.Error,
                        $"Failed to find concept {conceptDetails.ConceptName}, from the {conceptDetails.DocId} {Constants.DesignMd}, in {Constants.ExercisesJson} file"));
                }
            }

            return errors;
        }

        private Result SeverityToResult(Severity severity) =>
            severity switch
            {
                Severity.Error => Result.Errors,
                Severity.Fatal => Result.FatalError,
                Severity.None => Result.Success,
                _ => throw new ArgumentException($"unknown error Severity {severity}")
            };

        private void MergeLearningObjectives(ExerciseObjectTree exerciseObjectTree,
            LearningObjectives learningObjectives)
        {
            var concepts = exerciseObjectTree.Exercises.SelectMany(ex => ex.Concepts);
            foreach (var concept in concepts)
            {
                var objectives = learningObjectives.GetObjectivesForConcept(concept.Name);
                if (objectives != null)
                {
                    concept.LearningObjectives.Clear();
                    foreach (string objective in objectives)
                    {
                        concept.LearningObjectives.Add(objective);
                    }
                }
            }
        }
    }
}