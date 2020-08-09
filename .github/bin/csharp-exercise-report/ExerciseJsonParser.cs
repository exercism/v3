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

        public (Result result, ExerciseObjectTree, List<Error> errors)
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
                List<Error> errors = Validate(exerciseObjectTree);
                if (exerciseObjectTree.Exercises.Count == 0)
                {
                    var message = $"Json parser failed to parse input file starting {jsonText.Substring(0, 20)}";
                    return (
                        Result.FatalError,
                        exerciseObjectTree,
                        new List<Error> {new Error(ErrorSource.Exercise, Severity.Fatal, message)}
                    );
                }

                if (errors.Count > Constants.MaxErrors)
                {
                    errors.Add(new Error(ErrorSource.Exercise,
                        Severity.Error,
                        "Too many errors reading exercises.json - see exercise-errors.json"));
                }
                return (
                    errors.Count == 0
                        ? Result.Success
                        : errors.Count > Constants.MaxErrors
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

        private List<Error> Validate(ExerciseObjectTree exerciseObjectTree)
        {
            var output = exerciseObjectTree.Exercises.Select(ex => ValidateExercise(ex))
                .Where(exo => !string.IsNullOrWhiteSpace(exo))
                .Select(exo => new Error(ErrorSource.Exercise, Severity.Error, exo))
                .ToList();
            return output;
        }

        private string ValidateExercise(Exercise exercise)
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

        public string ErrorsToString(IList<Error> errors)
        {
            var options = new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true,
                WriteIndented = true,
            };
            options.Converters.Add(new JsonStringEnumConverter(JsonNamingPolicy.CamelCase));
            return JsonSerializer.Serialize(new ErrorReport(errors), options);
        }

        public ErrorReport ErrorsFromString(string errorsJson) {
            var options = new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true,
                WriteIndented = true,
            };
            options.Converters.Add(new JsonStringEnumConverter(JsonNamingPolicy.CamelCase));
            return JsonSerializer.Deserialize<ErrorReport>(errorsJson, options);
        }
}
    
    internal class ErrorReport
    {
        public IList<Error> Errors { get; set; } = new List<Error>();

        public ErrorReport(IList<Error> errors)
        {
            Errors = errors;
        }
    }
}