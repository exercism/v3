using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace ExerciseReport
{
    internal class DesignDocParser
    {
        private const string CONCEPT = "concept";
        private const string LEARNING_OBJECTIVE = "learningobjective";

        private static readonly Regex learningObjectiveRegex = new Regex(@$"^
            -\s                
            `(?<{CONCEPT}>.*)`                # e.g. `string-formatting`
            \s*:\s*                           # :
            (?<{LEARNING_OBJECTIVE}>.*)       # e.g. know about string formatting
            $",
            RegexOptions.IgnoreCase | RegexOptions.IgnorePatternWhitespace);

        // we are extracting the learning objectives as associated with each concept
        // not the ones actually called "Learning Objectives".  It is what it is.
        public IEnumerable<(Result Result, string Error,
            (string ExerciseName, string ConceptName) ConceptDetails,
            string Objective)> ParseDesignDoc(
            string exerciseName, string designDocContents)
        {
            string[] lines = designDocContents.Split("\n");
            var conceptsAndObjectives = lines
                .SkipWhile(line => !line.MatchesHeading("Concepts"))
                .Skip(1)
                .TakeWhile(line => !line.MatchesAnyHeading())
                .Where(line => line.Length > 1 && line[0] == '-' && char.IsWhiteSpace(line[1]))
                .SelectMany(line => LineToConceptAndObjectives(exerciseName, line))
                .DefaultIfEmpty((Result.Errors,
                    $"{exerciseName}: no learning objectives found",
                    (string.Empty, string.Empty), String.Empty));
            return conceptsAndObjectives;
        }

        // line: e.g. "- `basics`: basic-stuff; other-stuff"
        // => (true, "", basics, basic-stuff)
        // => (true, "", basics, other-stuff)
        private static List<(Result result, string error, (string DocId, string ConceptName), string objective)>
            LineToConceptAndObjectives(string designDocId, string line)
        {
            var match = learningObjectiveRegex.Match(line);
            if (match.Success && match.Groups.ContainsKey(CONCEPT) && match.Groups.ContainsKey(LEARNING_OBJECTIVE))
            {
                var results = new List<(Result result, string error, (string DesignDocId, string ConceptName), string objective)>();
                string conceptName = match.Groups[CONCEPT].Value;
                foreach (var learningObjective in match.Groups[LEARNING_OBJECTIVE].Value.Split(';'))
                {
                    results.Add((Result.Success, string.Empty, (designDocId, conceptName.Trim()), learningObjective.Trim()));
                }

                return results;
            }
            else
            {
                return new List<(Result result, string error, (string, string), string objective)>
                {
                    (Result.Errors,
                        $"{designDocId}: invalid format: {line}",
                        (string.Empty, string.Empty),
                        string.Empty)
                };
            }
        }

    }

    public static class ExtensionMethods
    {
        private const string HEADING_TEXT = "headingtext";
        private static readonly Regex headingRegex = new Regex(@$"
            ^\s*\#+\s                    # typically ##
            (?<{HEADING_TEXT}>.*)       # e.g one of the following: Concepts, Prerequisites
            $",
            RegexOptions.IgnoreCase | RegexOptions.IgnorePatternWhitespace);

        public static bool MatchesAnyHeading(this string line) => MatchesHeading(line);

        // line: e.g. "## Concepts" or "## Prerequisites"
        // headingText: specific text to match - "*" == any text
        public static bool MatchesHeading(this string line, string headingText = "*")
        {
            var match = headingRegex.Match(line.Trim());
            return match.Success switch
            {
                false => false,
                true when headingText == "*" => true,
                true when match.Groups[HEADING_TEXT].Value == headingText => true,
                _ => false
            };
        }
    }
}