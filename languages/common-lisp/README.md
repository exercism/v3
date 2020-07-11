# Common Lisp

Welcome to the workspace for Exercism v3's Common Lisp track!

This area will contain everything needed to launch the Common Lisp track, including:

- The new exercises and `config.json` file.
- Reference documentation that help explain Common Lisp concepts for which no appropriate online document could be found.
- Common Lisp-specific documentation for contributors.

## Preparation Status

Before we publicize requesting contribution for this language, the following steps should be done.

- [x] Have a kick-off discussion between track maintainers
- [x] Fill out the [maintainers.md](./maintainers.md) file (e.g. [C#](../csharp/maintainers.md))
- [x] Ensure there is a link to your track's GitHub issues on the [main README.md](../../README.md)
- [x] [Write a Concept Exercise implementation guide](../../docs/maintainers/writing-a-concept-exercise-github-issue.md)
- [x] [List out key Concepts for your language](../../docs/maintainers/determining-concepts.md)
- [x] [Add GitHub issues for 20 Concept Exercises](../../docs/maintainers/writing-a-concept-exercise-github-issue.md)

## Readiness for Launch

Before launch, we need all of the following parts to be completed:

### config.json

- [x] Added `version` key
- [x] Added online editor settings
  - [x] Added `indent_style`
  - [x] Added `indent_size`
- [ ] Convert the `exercises` array to an object
- [ ] Remove the `foregone` property

See the [migrating your config.json files document](../../docs/maintainers/migrating-your-config-json-files.md) for more information.

### Concept Exercises

- [ ] Added 20+ Concept Exercises

### Representer

- [ ] Build Representer
- [ ] Deploy Representer

### Test Runner

- [x] Build Test Runner
- [ ] Deploy Test Runner

## Extra magic

These extra steps will make your track better, but are optional.

### Analyzer

- [ ] Build Analyzer
- [ ] Deploy Analyzer
