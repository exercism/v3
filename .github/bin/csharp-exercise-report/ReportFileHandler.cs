using System.IO;

namespace ExerciseReport
{
    public interface IReportFileHandler
    {
        void WriteFile(string reportMarkdown);

    }

    public class ReportFileHandler : IReportFileHandler
    {
        private readonly string reportPathAndFileName;

        public ReportFileHandler(string root, string track)
        {
            reportPathAndFileName = Path.Combine(
                root,
                PathNames.Default.Languages,
                track,
                PathNames.Default.ConceptReport);
        }
        
        public void WriteFile(string reportMarkdown)
        {
            File.WriteAllText(reportPathAndFileName, reportMarkdown);
        }
    }
}