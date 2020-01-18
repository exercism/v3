# JavaScript

Welcome to the workspace for Exercism v3's JavaScript track!

This area will contain everything needed to launch the JavaScript track, including:

- The new exercises and `config.json` file.
- Reference documentation that help explain JavaScript concepts for which no appropriate online document could be found.
- JavaScript-specific documentation for contributors.

To discuss the overall roadmap, go [here](https://github.com/exercism/v3/issues/1).

## Preparation Status

Before we publicize requesting contribution for this language, the following steps should be done.

- [X] [Convert existing files to new repository structure](../../docs/maintainers/repository-structure.md)
- [ ] Have a kick-off discussion between track maintainers
- [x] Fill out the [maintainers.md](./maintainers.md) file (e.g. [C#](../csharp/maintainers.md))
- [x] Ensure there is a link to your track's GitHub issues on the [main README.md](../../README.md)
- [x] [Write a Concept Exercise implementation guide](../../docs/maintainers/writing-a-concept-exercise-github-issue.md)
- [x] [List out key Concepts for your language](../../docs/maintainers/determining-concepts.md)
- [ ] [Add GitHub issues for 20 Concept Exercises](../../docs/maintainers/writing-a-concept-exercise-github-issue.md)

## Readiness for Launch

Before launch, we need all of the following parts to be completed:

### Track Structure

- [ ] Implemented 20+ Concept Exercises
- [ ] [Updated `config.json`](../../docs/maintainers/migrating-your-config-json-files.md)
  - [x] Added `version` key
  - [x] Added online editor settings
    - [x] Added `indent_style`
    - [x] Added `indent_size`
  - [ ] Added Concept Exercises
  - [ ] Added Concepts for all Practice Exercises

### Representer

- [X] Build Representer
- [X] Deploy Representer

### Test Runner

- [X] Build Test Runner
- [ ] Deploy Test Runner

## Extra magic

These extra steps will make your track better, but are optional.

### Analyzer

- [X] Build Analyzer
- [X] Deploy Analyzer
