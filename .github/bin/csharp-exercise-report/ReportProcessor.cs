using System;

namespace ExerciseReport
{
    internal class ReportProcessor
    {
        public void Process(ExerciseMerger merger, ReportWriter reporter, ErrorWriter errorWriter)
        {
            var mergeResults = merger.MergeExercisesAndLearningObjectives();
            errorWriter.Write(mergeResults.Errors);
            
            if (mergeResults.Result == Result.FatalError)
            {
                throw new Exception("Failed to produce report: " + mergeResults.Errors[^1].Message);
            }

            reporter.WriteReport(mergeResults.ExerciseObjectTree);
        }
        
    }
}