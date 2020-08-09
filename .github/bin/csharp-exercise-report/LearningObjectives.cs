using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;

namespace ExerciseReport
{
    internal class LearningObjectives
    {
        public IBuilder Builder { get; }

        private readonly Dictionary<(string, string), List<string>> concepts = new Dictionary<(string, string), List<string>>();

        public interface IBuilder
        {
            void Add((string docId, string conceptName) concept, string learningObjective);
        }

        private class BuilderImpl : IBuilder
        {
            private readonly LearningObjectives _this;

            public BuilderImpl(LearningObjectives _this)
            {
                this._this = _this;
            }

            public void Add((string docId, string conceptName) concept, string learningObjective)
            {
                if (!_this.concepts.ContainsKey(concept))
                {
                    _this.concepts[concept] = new List<string>();
                }

                _this.concepts[concept].Add(learningObjective);
            }
        }

        public LearningObjectives()
        {
            Builder = new BuilderImpl(this);
        }

        public IEnumerable<string>? GetObjectivesForConcept(string conceptName)
        {
            if (!concepts.ContainsKey(("bob", conceptName)))
            {
                return null;
            }

            return new ReadOnlyCollection<string>(concepts[("bob", conceptName)]);
        }
        public IEnumerable<string> Concepts => concepts.Keys.Select(k => k.Item2);
        public IEnumerable<(string, string)> ConceptsInclDocId => concepts.Keys;
    }
}