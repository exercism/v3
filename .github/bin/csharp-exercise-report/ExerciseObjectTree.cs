using System.Collections.Generic;
using System.Text.Json.Serialization;

namespace ExerciseReport
{
    public enum DocumentType
    {
        None = 'N',
        Design = 'D',
        Issue = 'I',
        Invalid = 'L'
    }

    public enum Level
    {
        Introductory = 'A',
        Essential = 'B',
        Advanced = 'C',
        None = 'N',
        Invalid = 'L'
    }

    public class Exercise
    {
        [JsonPropertyName("slug")]
        public string Slug { get; set; } = string.Empty;
        [JsonPropertyName("level")]
        public Level Level { get; set; } = Level.Invalid;

        [JsonPropertyName("document-type")]
        public DocumentType DocumentType { get; set; } = DocumentType.Invalid;
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
        [JsonPropertyName("learning-objectives")]
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
        public List<string> Documentation => new List<string>{
            "This file is the original source of the concept report in csharp/reference/README.md.",
            "",
            "All the information originates here EXCEPT the learning-objectives array which is a part of each concept.",
            "Do NOT edit the learning-objectives array (or these lines of documentation)",
            "and do NOT attempt to change the schema in any way."
            };
        [JsonPropertyName("exercises")]
        public IList<Exercise> Exercises { get; set; } = new List<Exercise>(); 
    }
}
