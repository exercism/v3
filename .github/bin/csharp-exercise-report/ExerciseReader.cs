using System;
using System.Collections.Generic;

namespace ExerciseReport
{
    internal class ExerciseReader
    {
        public static ExerciseReader CSharpExerciseReader { get; } =
            new ExerciseReader(
                new ExerciseFileHandler(PathNames.Default.Root, Constants.CSharpTrack),
                new ExerciseJsonParser());

        private readonly IExerciseFileHandler exerciseFileHandler;
        private readonly ExerciseJsonParser exerciseJsonParser;

        public ExerciseReader(IExerciseFileHandler exerciseFileHandler,
            ExerciseJsonParser exerciseJsonParser)
        {
            this.exerciseFileHandler = exerciseFileHandler;
            this.exerciseJsonParser = exerciseJsonParser;
        }

        public (Result Result, ExerciseObjectTree ExerciseObjectTree, List<Error> Errors)
            ReadExercises()
        {
            try
            {
                var exerciseJson = exerciseFileHandler.ReadFile();
                return exerciseJsonParser.FromString(exerciseJson);
            }
            catch (Exception e)
            {
                return (
                    Result.FatalError,
                    new ExerciseObjectTree(),
                    new List<Error>
                        {new Error(ErrorSource.Process, Severity.Fatal, "reading exercise.json file: " + e.Message)}
                );
            }
        }
    }
}