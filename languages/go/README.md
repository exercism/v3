# Go

Welcome to the workspace for Exercism v3's Go track!

This area will contain everything needed to launch the Go track, including:

- The new exercises and `config.json` file.
- Reference documentation that help explain Go concepts for which no appropriate online document could be found.
- Go-specific documentation for contributors.

## Links

[:bar_chart: Project Board](https://github.com/exercism/v3/projects/4?fullscreen=true)

[:books: Open Issues](https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%3Atrack%2Fgo) |
[:gift: Open Pull Requests](https://github.com/exercism/v3/pulls?q=is%3Apr+is%3Aopen+label%3Atrack%2Fgo)

[:house_with_garden: v3 Docs Website](https://exercism.github.io/v3/#/)

[:gear: Go Concept Reference](https://exercism.github.io/v3/#/languages/go/reference/README) | [:speech_balloon: Go Concept Discussion](https://github.com/exercism/v3/issues/167) | [:hammer: Go Exercise Discussion](https://github.com/exercism/v3/issues/212)
[:wrench: Go Track Maintenance](https://tracks.exercism.io/go)

## Preparation Status

Before we publicize requesting contribution for this language, the following steps should be done.

- [x] [Convert existing files to new repository structure](../../docs/maintainers/repository-structure.md)
- [x] Have a kick-off discussion between track maintainers
- [x] Fill out the [maintainers.md](./maintainers.md) file
- [x] Ensure there is a link to your track's GitHub issues on the [main README.md](../../README.md)
- [x] [Write a Concept Exercise implementation guide](../../docs/maintainers/writing-a-concept-exercise-github-issue.md)
- [x] [List out key Concepts for your language](../../docs/maintainers/determining-concepts.md)
- [x] [Add GitHub issues for 20 Concept Exercises](../../docs/maintainers/writing-a-concept-exercise-github-issue.md)

## Readiness for Launch

Before launch, we need all the following parts to be completed:

### Track Structure

- [ ] Implemented 20+ Concept Exercises
- [Migrated `config.json`](../../docs/maintainers/migrating-your-config-json-files.md)
  - [x] Added `version` key
  - [x] Added online editor settings
    - [x] Added `indent_style`
    - [x] Added `indent_size`
  - [ ] Added Concept Exercises
    - see [exercise issue](https://github.com/exercism/v3/issues/212) for status on the individual concept exercises

### [Representer](https://github.com/exercism/automated-analysis/blob/master/docs/representers/introduction.md)

- [ ] Build Representer
- [ ] Deploy Representer

### [Test Runner](https://github.com/exercism/automated-tests)

- [x] Build Test Runner
- [x] Deploy Test Runner

## Extra magic

These extra steps will make your track better, but are optional.

### [Analyzer](https://github.com/exercism/automated-analysis/blob/master/docs/about.md)

- [x] Build Analyzer
- [x] Deploy Analyzer

## Contributing

[Getting Started for Maintainers](https://exercism.github.io/v3/#/docs/maintainers/README)

[How to implement a Go Concept Exercise](https://exercism.github.io/v3/#/languages/go/reference/implementing-a-concept-exercise)

[Example Go Concept Exercise](https://github.com/exercism/v3/tree/master/languages/go/exercises/concept/strings)
