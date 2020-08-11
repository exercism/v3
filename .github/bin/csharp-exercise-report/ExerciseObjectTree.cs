using System;
using System.Collections.Generic;
using System.Text.Json.Serialization;
using static ExerciseReport.Utils;

namespace ExerciseReport
{
    public enum CompletionStatus
    {
        None,
        Complete,
        NewExerciseIssueRaised,
        Invalid
    }

    public enum Level
    {
        Introductory,
        Essential,
        Advanced,
        None,
        Invalid
    }

    public class Exercise
    {
        [JsonPropertyName("slug")]
        public string Slug { get; set; } = string.Empty;
        [JsonPropertyName("level")]
        public Level Level { get; set; } = Level.Invalid;

        [JsonPropertyName("completion-status")]
        public CompletionStatus CompletionStatus { get; set; } = CompletionStatus.Invalid;
        [JsonPropertyName("document-link")]
        public string DocumentLink { get; set; } = string.Empty;
        [JsonPropertyName("concepts")]
        public IList<Concept> Concepts { get; set; } = new List<Concept>();
    }

    public class Concept
    {
        [JsonPropertyName("name")]
        public string Name { get; set; } = string.Empty;
        [JsonPropertyName("track-neutral-concept")]
        public string TrackNeutralConcept { get; set; } = string.Empty;
        [JsonIgnore]
        public IList<string> LearningObjectives { get; set; } = new List<string>();
        [JsonPropertyName("original-concepts")]
        public IList<OriginalConcept> OriginalConcepts { get; set; } = new List<OriginalConcept>();
    }

    public class OriginalConcept
    {
        [JsonPropertyName("name")]
        public string Name { get; set; } = string.Empty;
        [JsonPropertyName("line-number")]
        public int LineNumber { get; set; } = 0;
    }
    
    public class ExerciseObjectTree
    {
        [JsonPropertyName("documentation")]
        public List<string> Documentation => new List<string>(
            GetResourceAsString(Constants.ExercisesJsonHeaderResource).Split(Environment.NewLine)
        );
        [JsonPropertyName("exercises")]
        public IList<Exercise> Exercises { get; set; } = new List<Exercise>(); 
    }
}
