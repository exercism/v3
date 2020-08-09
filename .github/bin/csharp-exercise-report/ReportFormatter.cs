using System;
using System.IO;
using System.Linq;
using System.Text;

namespace ExerciseReport
{
    internal class ReportFormatter
    {
        private readonly string root;
        private const string Relativity = "../../..";
                // distance from v3/languages/csharp/reference to v3/reference
                // for location of track neutral concepts README.md

        public ReportFormatter(string root)
        {
            this.root = root;
        }

        public string CreateReport(ExerciseObjectTree exerciseObjectTree)
        {
            var concepts = CreateConceptPart(exerciseObjectTree);
            var conceptDefiniitons = CreateLearningObjectivesPart(exerciseObjectTree);
            var linkReferences = CreateLinkReferences(exerciseObjectTree);
            return concepts + conceptDefiniitons + linkReferences;
        }

        private string CreateConceptPart(ExerciseObjectTree exerciseObjectTree)
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendLine("# C&#35; reference");
            sb.AppendLine();
            sb.AppendLine("## Concepts");
            sb.AppendLine();
            sb.AppendLine(
                "The C# concept exercises are based on concepts. The list below contains the concepts that have been identified for the C# language.");
            sb.AppendLine();
            sb.AppendLine(
                @"_(Please do not modify this document. it is automatically generated. All text except the concept learning objectives is sourced
                            from [exercises.json](./exercises.json) which should be updated manually when a concept is added or an issue or new design is created
                            and learning objectives are scraped from the concept definition text in each exercise's design.md document)_.");
            sb.AppendLine();
            sb.AppendLine("### Introductory Concepts");
            sb.AppendLine(); 
            GetConcepts(sb, exerciseObjectTree, Level.Introductory);
            sb.AppendLine();
            sb.AppendLine("### Essential Concepts");
            sb.AppendLine(); 
            GetConcepts(sb, exerciseObjectTree, Level.Essential);
            sb.AppendLine();
            sb.AppendLine("### Advanced Concepts");
            sb.AppendLine(); 
            GetConcepts(sb, exerciseObjectTree, Level.Advanced);
            sb.AppendLine();
            sb.AppendLine("### Unallocated Concepts");
            sb.AppendLine(); 
            GetConcepts(sb, exerciseObjectTree, Level.None);
            return sb.ToString();
        }

        private string CreateLearningObjectivesPart(ExerciseObjectTree exerciseObjectTree)
        {
            var conceptList = exerciseObjectTree.Exercises
                .SelectMany(ex => ex.Concepts)
                .Where(c => c.LearningObjectives.Count > 0)
                .OrderBy(c => c.Name)
                .Select(c => new
                    {name = "`" + c.Name + "`",
                        learningObjectives = "<ul><li>" + string.Join("<li>", c.LearningObjectives)
                            })
                .ToList();
            if (conceptList.Count == 0)
            {
                return string.Empty;
            }
            int longestConceptName = conceptList.Max(c => c.name.Length);
            int longestLearningObjectives = conceptList.Max(c => c.learningObjectives.Length);
            StringBuilder sb = new StringBuilder();
            var format = "| {0,-" + longestConceptName + "} | {1,-" + longestLearningObjectives + "} |" +Environment.NewLine;
            sb.AppendLine();
            sb.AppendLine("## Learning Objectives");
            sb.AppendLine();
            sb.AppendFormat(format, "Concept", "Learning Objectives");
            sb.AppendFormat(format, new String('-', longestConceptName), new String('-', longestLearningObjectives));

            foreach (var text in conceptList)
            {
                sb.AppendFormat(format, text.name, text.learningObjectives);
            }
            return sb.ToString();
        }

        private string CreateLinkReferences(ExerciseObjectTree exerciseObjectTree)
        {
            StringBuilder sb = new StringBuilder();
            var issuesOrDesigns = exerciseObjectTree.Exercises
                .Where(ex => ex.DocumentType != DocumentType.None)
                .OrderBy(ex => ex.Slug)
                .Select(ex => $"[{(ex.DocumentType == DocumentType.Issue ? "issue-" : "design-") + ex.Slug}]: {ex.DocumentLink}");
 
            sb.AppendLine();
            foreach (string issueOrDesign in issuesOrDesigns)
            {
                sb.AppendLine(issueOrDesign);
            }

            var exerciseLocations = exerciseObjectTree.Exercises
                .Where(ex => ex.DocumentType == DocumentType.Design)
                .OrderBy(ex => ex.Slug)
                .Select(ex => $"[exercise-{ex.Slug}]: {GetExerciseLocationLink(ex.Slug)}");
 
            sb.AppendLine();
            foreach (string exerciseLocation in exerciseLocations)
            {
                sb.AppendLine(exerciseLocation);
            }

            var trackNeutralConcepts = exerciseObjectTree.Exercises
                .SelectMany(ex => ex.Concepts)
                .Where(c => !string.IsNullOrWhiteSpace(c.TrackNeutralConcept))
                .OrderBy(c => c.Name)
                .Select(c => $"[tnc-{c.Name}]: {Path.Combine(root, Relativity, c.TrackNeutralConcept)}");

            sb.AppendLine();
            foreach (string trackNeutralConcept in trackNeutralConcepts)
            {
                sb.AppendLine(trackNeutralConcept);
            }
            return sb.ToString();
        }

        private string GetExerciseLocationLink(string exerciseSlug)
        {
            return Path.Combine(
                "..",
                PathNames.Default.Exercises,
                exerciseSlug
            );
        }

        private void GetConcepts(StringBuilder sb, ExerciseObjectTree exerciseObjectTree, Level level)
        {
            var outputs = exerciseObjectTree.Exercises
                .SelectMany(ex => ex.Concepts, (ex,
                    c) => new {Exercise = ex, Concept = c})
                .Where(p => p.Exercise.Level == level)
                .OrderBy(p => p.Concept.Name)
                .Select(p => FormatConceptReportLine(p.Exercise, p.Concept)).DefaultIfEmpty("None");
            foreach (string output in outputs)
            {
                sb.AppendLine(output);
            }
        }

        // e.g. "arrays (arrays) - DesignDocName, Background" 
        private string FormatConceptReportLine(Exercise exercise, Concept concept)
        {
            var link = (exercise.DocumentType, concept.TrackNeutralConcept) switch
            {
                (DocumentType.Issue, "") => $" - [Issue][issue-{exercise.Slug}]",
                (DocumentType.Design, "") => $" - [Design][design-{exercise.Slug}]",
                (DocumentType.Issue, _) => $" - [Issue][issue-{exercise.Slug}], [Background][tnc-{concept.Name}]",
                (DocumentType.Design, _) => $" - [Design][design-{exercise.Slug}], [Background][tnc-{concept.Name}]",
                (DocumentType.None, "") => string.Empty,
                _ => $" - [Background][tnc-{concept.Name}]"
            };
            string exerciseText = $"_({exercise.Slug})_";
            if (exercise.DocumentType == DocumentType.Design)
            {
                exerciseText = $"[_({exercise.Slug})_][exercise-{exercise.Slug}]";
            }
            return $"- {concept.Name} {exerciseText}{link}";
        }
    }
}