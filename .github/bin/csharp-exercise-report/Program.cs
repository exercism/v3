using System;
using System.IO;

namespace ExerciseReport
{
    public class Program
    {
        public static int Main(string[] args)
        {
            try
            {
                // usage: dotnet run # in production no param is required
                // usage: dotnet run [/myProjects/exercism/v3]
                if (args.Length > 0)
                {
                    Directory.SetCurrentDirectory(args[0]);
                        // e.g. /Users/mikedamay/projects/exercism/v3
                }
                var merger = ExerciseMerger.CSharpMerger;
                merger.MergeInLearningObjectives();
                var reporter = ReportCollator.CSharpReportCollator;
                var efc = ExerciseFileCollator.CSharpExerciseFileCollator;
                var outputs = efc.ReadExercises();
                if (outputs.Result == Result.FatalError)
                {
                    throw new Exception("Failed to produce report: " + outputs.Errors[^1].Message);
                }
                reporter.WriteReport(outputs.ExerciseObjectTree);
                return 0;
            }
            catch (Exception e)
            {
                Console.Out.WriteLine(e.Message);
                return 1;
            }
        }
    }
}