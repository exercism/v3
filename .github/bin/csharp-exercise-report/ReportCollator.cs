namespace ExerciseReport
{
    internal class ReportCollator
    {
        private readonly IReportFileHandler reportFileHandler;
        private readonly ReportFormatter reportFormatter;

        public static ReportCollator CSharpReportCollator { get; } =
            new ReportCollator(new ReportFileHandler(PathNames.Default.Root, Constants.CSharpTrack),
                new ReportFormatter(PathNames.Default.Root));
        public ReportCollator(IReportFileHandler reportFileHandler,
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