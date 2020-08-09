using System;
using System.Collections.Generic;

namespace ExerciseReport
{
    internal class ExerciseFileCollator
    {
        public static ExerciseFileCollator CSharpExerciseFileCollator { get; }=
            new ExerciseFileCollator(
                new ExerciseFileHandler(PathNames.Default.Root, Constants.CSharpTrack),
                new ExerciseJsonParser());

        private readonly IExerciseFileHandler exerciseFileHandler;
        private readonly ExerciseJsonParser exerciseJsonParser;

        public ExerciseFileCollator(IExerciseFileHandler exerciseFileHandler,
            ExerciseJsonParser exerciseJsonParser)
        {
            this.exerciseFileHandler = exerciseFileHandler;
            this.exerciseJsonParser = exerciseJsonParser;
        }

        public (Result result, ExerciseObjectTree exerciseObjectTree, List<Error> errors) 
            ReadExercises()
        {
            try
            {
                var text = exerciseFileHandler.ReadExerciseFile();
                return exerciseJsonParser.FromString(text);
            }
            catch (Exception e)
            {
                return (
                    Result.FatalError,
                    new ExerciseObjectTree(),
                    new List<Error>{new Error(ErrorSource.Process, Severity.Fatal, "reading exercise.json file: " + e.Message)}
                    );
            }
        }

        public void WriteExercises(Result result, ExerciseObjectTree exerciseObjectTree, IList<Error> errors)
        {
            try
            {
                if (result != Result.FatalError)
                {
                    var exerciseJson = exerciseJsonParser.ToString(exerciseObjectTree);
                    exerciseFileHandler.WriteExerciseFile(exerciseJson);
                }
                var errorsJson = exerciseJsonParser.ErrorsToString(errors);
                exerciseFileHandler.WriteErrorFile(errorsJson);
            }
            catch (Exception e)
            {
                var _ = e;
                throw;
            }
        }
    }
}