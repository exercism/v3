using System;
using System.Collections.Generic;

namespace ExerciseReport
{
    internal class ErrorWriter
    {
        public static ErrorWriter CSharpErrorWriter { get; }=
            new ErrorWriter(
                new ErrorFileHandler(PathNames.Default.Root, Constants.CSharpTrack),
                new ErrorJsonParser());

        private readonly IErrorFileHandler fileHandler;
        private readonly ErrorJsonParser jsonParser;

        public ErrorWriter(IErrorFileHandler fileHandler,
            ErrorJsonParser jsonParser)
        {
            this.fileHandler = fileHandler;
            this.jsonParser = jsonParser;
        }

        public void Write(IList<Error> errors)
        {
            try
            {
                var errorsJson = jsonParser.ToString(errors);
                fileHandler.WriteFile(errorsJson);
            }
            catch (Exception e)
            {
                var _ = e;
                throw;
            }
        }
    }
}