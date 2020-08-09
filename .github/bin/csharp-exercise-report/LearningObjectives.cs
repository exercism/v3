using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace ExerciseReport
{
    internal class LearningObjectives
    {
        public IBuilder Builder { get; }

        private readonly Dictionary<(string, string), List<string>> conceptsInclDesignDocId 
            = new Dictionary<(string, string), List<string>>();
        private readonly Dictionary<string, List<string>> concepts = new Dictionary<string, List<string>>();

        public interface IBuilder
        {
            void Add((string DesignDocId, string ConceptName) concept, string learningObjective);
        }

        private class BuilderImpl : IBuilder
        {
            private readonly LearningObjectives _this;

            public BuilderImpl(LearningObjectives _this)
            {
                this._this = _this;
            }

            public void Add((string DesignDocId, string ConceptName) concept, string learningObjective)
            {
                AddConceptInclDesignDocId(concept, learningObjective);
                AddConcept(concept.ConceptName, learningObjective);
            }

            private void AddConcept(string conceptName, string learningObjective)
            {
                if (!_this.concepts.ContainsKey(conceptName))
                {
                    _this.concepts[conceptName] = new List<string>();
                }

                _this.concepts[conceptName].Add(learningObjective);
            }

            private void AddConceptInclDesignDocId((string, string) concept, string learningObjective)
            {
                if (!_this.conceptsInclDesignDocId.ContainsKey(concept))
                {
                    _this.conceptsInclDesignDocId[concept] = new List<string>();
                }

                _this.conceptsInclDesignDocId[concept].Add(learningObjective);
            }
        }

        public LearningObjectives()
        {
            Builder = new BuilderImpl(this);
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