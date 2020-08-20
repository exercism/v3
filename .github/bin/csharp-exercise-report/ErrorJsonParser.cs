using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace ExerciseReport
{
    internal class ErrorJsonParser
    {
        public string ToString(IList<Error> errors)
        {
            var options = new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true,
                WriteIndented = true,
            };
            options.Converters.Add(new JsonStringEnumConverter(JsonNamingPolicy.CamelCase));
            return JsonSerializer.Serialize(new ErrorReport(errors), options);
        }

        public ErrorReport FromString(string errorsJson) {
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

        public ErrorReport()
        {
            
        }
    }
}