using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace ExerciseReport
{
    internal class ExerciseJsonParser
    {
        private readonly int maxErrors;

        public ExerciseJsonParser(int maxErrors = Constants.MaxErrors)
        {
            this.maxErrors = maxErrors;
        }
        public string ToString(ExerciseObjectTree exerciseObjectTree)
        {
            var options = new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true,
                WriteIndented = true,
            };
            options.Converters.Add(new JsonStringEnumConverter(JsonNamingPolicy.CamelCase));
            return JsonSerializer.Serialize(exerciseObjectTree, options);
        }

        public (Result Result, ExerciseObjectTree, List<Error> Errors)
            FromString(string jsonText)
        {
            var options = new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true
            };
            try
            {
                options.Converters.Add(new JsonStringEnumConverter(JsonNamingPolicy.CamelCase));
                var exerciseObjectTree = JsonSerializer.Deserialize<ExerciseObjectTree>(jsonText, options);
                List<Error> errors = ValidateExercises(exerciseObjectTree);
                if (exerciseObjectTree.Exercises.Count == 0)
                {
                    var message = $"Json parser failed to parse input file starting {jsonText.Substring(0, 20)}";
                    return (
                        Result.FatalError,
                        exerciseObjectTree,
                        new List<Error> {new Error(ErrorSource.Exercise, Severity.Fatal, message)}
                    );
                }

                if (errors.Count > maxErrors)
                {
                    errors.Add(new Error(ErrorSource.Exercise,
                        Severity.Error,
                        $"Too many errors reading {Constants.ExercisesJson} - see {Constants.ExerciseErrorsJson}"));
                }
                return (
                    errors.Count == 0
                        ? Result.Success
                        : errors.Count > maxErrors
                            ? Result.FatalError
                            : Result.Errors,
                    exerciseObjectTree,
                    errors
                );
            }
            catch (JsonException je)
            {
                return (
                    Result.FatalError,
                    new ExerciseObjectTree(),
                    new List<Error> {new Error(ErrorSource.Exercise, Severity.Fatal, je.Message)}
                );
            }
            catch (Exception e)
            {
                return (
                    Result.FatalError,
                    new ExerciseObjectTree(),
                    new List<Error> {new Error(ErrorSource.Exercise, Severity.Fatal, "unknown error:" + e.Message)}
                );
            }
        }

        private static List<Error> ValidateExercises(ExerciseObjectTree exerciseObjectTree)
        {
            var errors = exerciseObjectTree.Exercises.Select(ex => ValidateExercise(ex))
                .Where(exo => !string.IsNullOrWhiteSpace(exo))
                .Select(exo => new Error(ErrorSource.Exercise, Severity.Error, exo))
                .ToList();
            return errors;
        }

        private static string ValidateExercise(Exercise exercise)
        {
            StringBuilder sb = new StringBuilder();
            
            if (string.IsNullOrWhiteSpace(exercise.Slug)) sb.AppendLine("slug: missing for an exercise");
            
            if (exercise.Level == Level.Invalid) sb.AppendLine($"level: missing for {exercise.Slug}");
            
            if (exercise.CompletionStatus == CompletionStatus.Invalid)
                sb.AppendLine($"completion-status: missing for {exercise.Slug}");
            
            if (exercise.CompletionStatus == CompletionStatus.NewExerciseIssueRaised
                && string.IsNullOrWhiteSpace(exercise.DocumentLink))
                sb.AppendLine($"document-link: missing for {exercise.Slug}");
            
            if (exercise.CompletionStatus == CompletionStatus.Complete
                && !string.IsNullOrWhiteSpace(exercise.DocumentLink))
                sb.AppendLine($"document-link: present for {exercise.Slug}. This will be ignored when generating the report");
            
            if (exercise.Concepts.Count == 0) sb.AppendLine($"concepts: missing for {exercise.Slug}");

            for (int ii = 0; ii < exercise.Concepts.Count; ii++)
            {
                if (string.IsNullOrWhiteSpace(exercise.Concepts[ii].Name))
                    sb.AppendLine($"concept.name: missing for {exercise.Slug}");
            }

            return sb.ToString();
        }
    }
}