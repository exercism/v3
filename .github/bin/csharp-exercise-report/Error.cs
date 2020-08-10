namespace ExerciseReport
{
    internal enum Result 
    {
        Success,
        Errors,
        FatalError
    }

    internal enum Severity
    {
        None,
        Error,
        Fatal
    }

    internal enum ErrorSource 
    {
        Process,
        Design,
        Exercise,
        Merge,
        None,
        MissingLearningObjective
    }

    internal class Error
    {
        public Severity Severity { get; set; } = Severity.None;
        public string Message { get; set; } = string.Empty;
        public ErrorSource Source { get; set; } = ErrorSource.None;

        public Error(ErrorSource source, Severity severity, string message)
        {
            Source = source;
            Severity = severity;
            Message = message;
        }

        public Error()
        {
            
        }
    }
}