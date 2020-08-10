using System;
using System.Collections.Generic;
using System.Linq;

namespace ExerciseReport
{
    internal class ExerciseMerger
    {
        private readonly ExerciseFileCollator exerciseFileHandler;
        private readonly DesignDocCollator designDocCollator;
        private readonly int maxErrors;

        public ExerciseMerger(ExerciseFileCollator exerciseFileHandler,
            DesignDocCollator designDocCollator,
            int maxErrors = Constants.MaxMissingLearningObjectives)
        {
            this.exerciseFileHandler = exerciseFileHandler;
            this.designDocCollator = designDocCollator;
            this.maxErrors = maxErrors;
        }

        public static ExerciseMerger CSharpMerger { get; } =
            new ExerciseMerger(new ExerciseFileCollator(
                    new ExerciseFileHandler(PathNames.Default.Root, Constants.CSharpTrack), 
                    new ExerciseJsonParser())
                , new DesignDocCollator(
                    new DesignDocFileHandler(PathNames.Default.Root, Constants.CSharpTrack),
                    new DesignDocParser())
                );

        public void MergeInLearningObjectives()
        {
            var mergeResults = Merge();
            exerciseFileHandler.WriteExercises(mergeResults.Result,
                mergeResults.ExerciseObjectTree, mergeResults.Errors);
        }

        private (Result Result, ExerciseObjectTree ExerciseObjectTree, List<Error> Errors) 
            Merge()
        {
            var outputs = exerciseFileHandler.ReadExercises();
            if (outputs.Result == Result.FatalError)
            {
                return outputs;
            }
            var learningObjectives = designDocCollator.GetAllLearningObjectives();
            MergeLearningObjectives(outputs.ExerciseObjectTree, learningObjectives.learningObjectives);
            var unmatchedConcepts = ReportUnmatchedConcepts(outputs.ExerciseObjectTree, learningObjectives.learningObjectives);
            var missingLearningObjectives = ReportMissingLearningObjectives(outputs.ExerciseObjectTree);
            var combinedErrors = outputs.Errors
                .Concat(learningObjectives.errors)
                .Concat(unmatchedConcepts)
                .Concat(missingLearningObjectives)
                .ToList();
            var maxSeverity = combinedErrors.Select(e => e.Severity).DefaultIfEmpty(Severity.None).Max();
            Result result = SeverityToResult(maxSeverity);
            return (result, outputs.ExerciseObjectTree, combinedErrors);
        }

        private List<Error> ReportMissingLearningObjectives(ExerciseObjectTree exerciseObjectTree)
        {
            var errors = exerciseObjectTree.Exercises.Where(ex => ex.CompletionStatus == CompletionStatus.Complete)
                .SelectMany(ex => ex.Concepts)
                .Where(con => con.LearningObjectives.Count == 0)
                .Select( con =>
                    new Error(ErrorSource.MissingLearningObjective,
                        Severity.Error,
                        $"The {con.Name} concept has no learning objectives in exercises.json")
                ).ToList();
            if (errors.Count > maxErrors)
            {
                errors.Add(
                    new Error(ErrorSource.MissingLearningObjective, 
                        Severity.Fatal,
                        "Too many concepts have no learning objectives - see exercise-errors.json")
                    );
            }

            return errors;
        }

        private List<Error> ReportUnmatchedConcepts(ExerciseObjectTree exerciseObjectTree,
            LearningObjectives learningObjectives)
        {
            var exerciseMap = exerciseObjectTree.Exercises
                .SelectMany(ex =>  ex.Concepts)
                .Select(con => con.Name)
                .ToHashSet();
            
            List<Error> errors = new List<Error>();
            foreach ((string DocId, string ConceptName) conceptDetails in learningObjectives.ConceptsInclDesignDocId)
            {
                if (!exerciseMap.Contains(conceptDetails.ConceptName))
                {
                    errors.Add(new Error(ErrorSource.Merge, Severity.Error,
                        $"Failed to find concept {conceptDetails.ConceptName}, from the {conceptDetails.DocId} design.md, in exercises.json file"));
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