namespace ExerciseReport
{
    internal enum Result 
    {
        Success = 0,
        Errors = 1,
        FatalError = 2
    }

    internal enum Severity
    {
        None = 0,
        Error = 1,
        Fatal = 2
    }

    internal enum ErrorSource 
    {
        Process,
        Design,
        Exercise,
        Merge
    }

    internal class Error
    {
        public Severity Severity { get; }
        public string Message { get; }
        public ErrorSource Source { get; }

        public Error(ErrorSource source, Severity severity, string message)
        {
            Source = source;
            Severity = severity;
            Message = message;
        }
    }
}