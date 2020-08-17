using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace ExerciseReport
{
    internal class LearningObjectives
    {
        public interface IBuilder
        {
            void Add((string DesignDocId, string ConceptName) concept, string learningObjective);
            LearningObjectives CreateLearningObjectives();
        }

        private class BuilderImpl : IBuilder
        {
            private readonly Dictionary<string, List<string>> concepts = new Dictionary<string, List<string>>();

            private readonly Dictionary<(string, string), List<string>> conceptsInclDesignDocId
                = new Dictionary<(string, string), List<string>>();

            public void Add((string DesignDocId, string ConceptName) concept, string learningObjective)
            {
                AddConceptInclDesignDocId(concept, learningObjective);
                AddConcept(concept.ConceptName, learningObjective);
            }

            public LearningObjectives CreateLearningObjectives()
            {
                return new LearningObjectives(concepts, conceptsInclDesignDocId);
            }

            private void AddConcept(string conceptName, string learningObjective)
            {
                if (!concepts.ContainsKey(conceptName))
                {
                    concepts[conceptName] = new List<string>();
                }

                concepts[conceptName].Add(learningObjective);
            }

            private void AddConceptInclDesignDocId((string, string) concept, string learningObjective)
            {
                if (!conceptsInclDesignDocId.ContainsKey(concept))
                {
                    conceptsInclDesignDocId[concept] = new List<string>();
                }

                conceptsInclDesignDocId[concept].Add(learningObjective);
            }
        }

        private readonly Dictionary<string, List<string>> concepts = new Dictionary<string, List<string>>();

        private readonly Dictionary<(string, string), List<string>> conceptsInclDesignDocId
            = new Dictionary<(string, string), List<string>>();

        public static IBuilder CreateBuilder()
        {
            return new BuilderImpl();
        }

        private LearningObjectives(Dictionary<string, List<string>> concepts,
            Dictionary<(string, string), List<string>> conceptsInclDesignDocId)
        {
            this.concepts = concepts;
            this.conceptsInclDesignDocId = conceptsInclDesignDocId;
        }

        public IEnumerable<string>? GetObjectivesForConcept(string conceptName)
        {
            if (!concepts.ContainsKey(conceptName))
            {
                return null;
            }

            return new ReadOnlyCollection<string>(concepts[conceptName]);
        }

        public IEnumerable<(string, string)> ConceptsInclDesignDocId => conceptsInclDesignDocId.Keys;
    }
}