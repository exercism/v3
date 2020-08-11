namespace ExerciseReport
{
    internal class ReportWriter
    {
        private readonly IReportFileHandler reportFileHandler;
        private readonly ReportFormatter reportFormatter;

        public static ReportWriter CSharpReportWriter { get; } =
            new ReportWriter(new ReportFileHandler(PathNames.Default.Root, Constants.CSharpTrack),
                new ReportFormatter(PathNames.Default.Root));
        public ReportWriter(IReportFileHandler reportFileHandler,
            ReportFormatter reportFormatter)
        {
            this.reportFileHandler = reportFileHandler;
            this.reportFormatter = reportFormatter;
        }

        public void WriteReport(ExerciseObjectTree exerciseObjectTree)
        {
            string markdown = reportFormatter.CreateReport(exerciseObjectTree);
            reportFileHandler.WriteFile(markdown);
        }
    }
}