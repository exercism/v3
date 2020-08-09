using System;
using System.IO;

namespace ExerciseReport
{
    public class Program
    {
        public static void Main(string[] args)
        {
            try
            {
                if (args.Length > 0)
                {
                    Directory.SetCurrentDirectory(args[0]);
                        // e.g. /Users/mikedamay/projects/exercism/v3
                }
                var merger = ExerciseMerger.CSharpMerger;
                merger.MergeInLearningObjectives();
                var reporter = ReportCollator.CSharpReportCollator;
                var efc = ExerciseFileCollator.CSharpExerciseFileCollator;
                var result = efc.ReadExercises();
                if (result.result == Result.FatalError)
                {
                    throw new Exception("Failed to produce report: " + result.errors[^1].Message);
                }
                reporter.WriteReport(result.exerciseObjectTree);
            }
            catch (Exception e)
            {
                Console.Out.WriteLine(e.Message);
            }
        }
    }
}