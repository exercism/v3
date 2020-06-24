# Python

Welcome to the workspace for Exercism v3's Python track!

This area will contain everything needed to launch the Python track, including:

- The new exercises and `config.json` file.
- Reference documentation that help explain Python concepts for which no appropriate online document could be found.
- Python-specific documentation for contributors.

## Preparation Status

Before we publicize requesting contribution for this language, the following steps should be done.

- [ ] [Convert existing files to new repository structure](../../docs/maintainers/repository-structure.md)
- [x] Have a kick-off discussion between track maintainers
- [x] Fill out the [maintainers.md](./maintainers.md) file (e.g. [C#](../csharp/maintainers.md))
- [x] Ensure there is a link to your track's GitHub issues on the [main README.md](../../README.md)
- [x] [Write a Concept Exercise implementation guide](../../docs/maintainers/writing-a-concept-exercise-github-issue.md)
- [ ] [List out key Concepts for your language](../../docs/maintainers/determining-concepts.md)
- [x] [Add GitHub issues for 20 Concept Exercises](../../docs/maintainers/writing-a-concept-exercise-github-issue.md)

## Readiness for Launch

Before launch, we need all of the following parts to be completed:

### Track Structure

- [ ] Implemented 20+ Concept Exercises
- [Migrated `config.json`](../../docs/maintainers/migrating-your-config-json-files.md)
  - [x] Added `version` key
  - [x] Added online editor settings
    - [x] Added `indent_style`
    - [x] Added `indent_size`
  - [ ] Added Concept Exercises

### Representer

- [x] Build [Representer](https://github.com/exercism/python-representer)
- [ ] Deploy Representer

### Test Runner

- [x] Build [Test Runner](https://github.com/exercism/python-test-runner)
- [x] Deploy Test Runner

## Extra magic

These extra steps will make your track better, but are optional.

### Analyzer

- [x] Build [Analyzer](https://github.com/exercism/python-analyzer)
- [x] Deploy Analyzer
