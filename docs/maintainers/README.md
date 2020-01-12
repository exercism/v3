# Maintainers - How to get started

Firstly, thank you for your willingness to help with Exercism. We believe that v3 will be a huge step forward in our aim to offer a comprehesive free learning website to anyone wanting to develop their programming skills, and we're really excited to see it play out.

**Before doing anything else, please create an issue in this repository asking for your track to be bootstrapped. You can then spend some time learning about the v3 process while we set things up for you.**

## Concept Exercises

The key things to understand about v3 is that we are making a strong distinction between Concept Exercises and Practice Exercises. All existing exercises will become Practice Exericses - exercises where students can practice the ideas they are learning, and discuss them with a mentor. The new Concept Exercises will be written from scratch, each with the aim of teaching one or more programming concepts that are necessary to become fluent in a given language.

We want to crowd-source the creation of these Concept Exercises, providing clear guidelines and instructions for contributors. We are doing this by asking maintainers to create a GitHub Issue for each Concept Exercise.

In order to do this maintainers need to understand what they think their tracks are going to look like. We have determined that the best way to do this is to start to list out all of the Concepts that someone needs to cover to learn a given language. Once this is done, you can start to turn each Concept into an issue that someone in the community can pick up and run with.

The final step to preparing your language to accept contributions is to provide a sample Concept Exercise that you have written. Not only does this act as an example for other contributors, it is also crucial for you understanding the process and getting your teeth into the detail.

We recommend choosing a Concept that involves working with basic types, like strings and numbers. Having chosen a Concept, look for the Concept's description in this reference part of this repository. This file should have references to all tracks that have implemented the exercise. You can use these existing implementations as a starting point for your first concept exercise, which you can then tailor to your specific language.

### Read more

- [More about Concepts and Concept Exercises](../concept-exercises.md)
- [How to determine Concepts for your track](./determining-concepts.md)
- [How to write Concept Exercise GitHub issues](./writing-a-concept-exercise-github-issue.md)
- [How the repository is structured](./repository-structure.md)

## Migrating your config.json files

For v3, the existing `config.json` files will need to be updated. You can use the [C# config.json](../../languages/csharp/config.json) for reference.

1. Add a version property:

```json
"version": 3
```

2. Add online editor settings:

TODO: Write up something about https://github.com/exercism/ruby/blob/master/config.json#L7

3. Convert the `"exercises"` array to an object with two properties:

```json
"exercises": {
  "concept": [],
  "practice": []
}
```

4. Added concepts for all Practice Exercises

The existing exercises are temporarily removed from the `config.json` file. They will return as practice exercises once the concept exercises have been added.

TODO: Discuss more about what needs to be done


## Tooling

As well as building out the Concept Exercises, your track will also need a Test Runner and a Representer.

The Test Runner is a piece of software that runs the tests. This enables us to have in-browser coding. Test Runners are surprisingly tricky to get right, with lots of weird edge cases and oddities, so we recommend starting on them quite early. You can find more in the [automated testing repository](https://github.com/exercism/automated-tests).

Representers are pieces of software that take a solution and provide a normalised representation of it. We can then attach feedback to those representations, and automatically provide the same feedback for future solutions that normalise to the same representation. You may also like to create an Analyzer, which automatically provides feedback based on heursitics provided by AST matching. You can learn more about Representers and Analysers in our [automated analysis repository](https://github.com/exercism/automated-analysis).

## Getting help

Each track has a #maintaining-x channel on [Slack](https://exercism-team.slack.com). We recommend using that to chat amongst yourselves.

If you need help, the best place to use is the [#maintainers](https://exercism-team.slack.com/archives/GC3K95MRR) channel on Slack. This is a private space where maintainers from all tracks can help each other.

If you need to work through something more complex, please message @ErikSchierboom on Slack and schedule a time to pair with him.
