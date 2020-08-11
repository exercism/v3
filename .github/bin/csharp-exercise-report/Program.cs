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
                var errorWriter = ErrorWriter.CSharpErrorWriter;
                var reporter = ReportWriter.CSharpReportWriter;
                new ReportProcessor().Process(merger, reporter, errorWriter);
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