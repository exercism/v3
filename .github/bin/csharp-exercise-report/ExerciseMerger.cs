using System;
using System.Collections.Generic;
using System.Linq;

namespace ExerciseReport
{
    internal class ExerciseMerger
    {
        private readonly ExerciseFileCollator exerciseFileHandler;
        private readonly DesignDocCollator designDocCollator;
        private readonly string track;

        public ExerciseMerger(string track,
            ExerciseFileCollator exerciseFileHandler, DesignDocCollator designDocCollator)
        {
            this.exerciseFileHandler = exerciseFileHandler;
            this.designDocCollator = designDocCollator;
            this.track = track;
        }

        public static ExerciseMerger CSharpMerger { get; } =
            new ExerciseMerger(Constants.CSharpTrack, 
                new ExerciseFileCollator(
                    new ExerciseFileHandler(PathNames.Default.Root, Constants.CSharpTrack), 
                    new ExerciseJsonParser())
                , new DesignDocCollator(
                    new DesignDocFileHandler(PathNames.Default.Root, Constants.CSharpTrack),
                    new DesignDocParser())
                );

        public void MergeInLearningObjectives()
        {
            var mergeResults = Merge();
            exerciseFileHandler.WriteExercises(mergeResults.result,
                mergeResults.exerciseObjectTree, mergeResults.errors);
        }

        public (Result result, ExerciseObjectTree exerciseObjectTree, List<Error> errors) 
            Merge()
        {
            var outputs = exerciseFileHandler.ReadExercises();
            if (outputs.result == Result.FatalError)
            {
                return outputs;
            }
            var learningObjectives = designDocCollator.GetAllLearningObjectivesForTrack(track);
            MergeLearningObjectives(outputs.exerciseObjectTree, learningObjectives.learningObjectives);
            var unmatchedConcepts = ReportUnmatchedConcepts(outputs.exerciseObjectTree, learningObjectives.learningObjectives);
            var combinedErrors = outputs.errors.Concat(learningObjectives.errors).Concat(unmatchedConcepts).ToList();
            var maxSeverity = combinedErrors.Select(e => e.Severity).DefaultIfEmpty(Severity.None).Max();
            Result result = SeverityToResult(maxSeverity);
            return (result, outputs.exerciseObjectTree, combinedErrors);
        }

        private List<Error> ReportUnmatchedConcepts(ExerciseObjectTree exerciseObjectTree,
            LearningObjectives learningObjectives)
        {
            var exerciseMap = exerciseObjectTree.Exercises
                .SelectMany(ex =>  ex.Concepts)
                .Select(con => con.Name)
                .ToHashSet();
            
            List<Error> errors = new List<Error>();
            foreach ((string DocId, string ConceptName) conceptDetails in learningObjectives.ConceptsInclDocId)
            {
                if (!exerciseMap.Contains(conceptDetails.ConceptName))
                {
                    errors.Add(new Error(ErrorSource.Merge, Severity.Error,
                        $"Failed to find concept {conceptDetails.ConceptName} from {conceptDetails.DocId} in exercises.json file"));
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
            foreach (Concept concept in concepts)
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