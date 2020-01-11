# How is this repository structured?

This repository can be viewed both via GitHub and via the v3 website. This page describes the repository as found on GitHub. It is roughly equivalent to the website, with filenames being titelized.

Things are grouped into several subdirectories:

## [Docs][docs]

This directory contains files for both the public, and maintainers, explaining what v3 is and how to contribute.

## [Reference][reference]

A list of reference documents for the various CS concepts, types, paradigms, etc.

- **concepts:** This directory contains files discussing **programming** concepts. These documents should provide guidance, limitations, and links for maintainers who need to expound those ideas on their tracks.
- **paradigms** This directory contains files discussing programming paradigms. These documents should link to the concepts typically associated with the paradigms.
- **tooling** This directory contains documents on tooling that is common across languages (either due to a shared ecosystem - such as JS/TS/Clojurescript - or a shared platform - such as .net or JVM).
- **types:** This directory contains files discussing programming types. These documents should provide guidance, limitations, and links for maintainers who need to expound those ideas on their tracks.

## [Languages][languages]

Each track has it's own directory containing a README.md that outlines the concepts that are necessary to learn to become fluent in those languages. Each language has a `config.json` file and the following subdirectories:

- **config.json** The WIP config.json for a track, with the practice exercises removed to avoid noise.
- **docs:** This directory contains files for both the public, and maintainers, describing the C# v3 track and how to contribute.
- **exercises:** The WIP exercises for that language.
  - **.docs:** documentation shared across all exercises for that language.
  - **concept:** The WIP concept exercises for that language.
  - **practice:** The WIP practice exercises for that language.
- **README.md:** The track's README, which should contain a short description of the goal of the repository as well as the status of the track's v3 transition.
- **reference:** Files that help maintainers develop the track. These could be reference documents that help explain track-specific concepts for which no appropriate online document could be found, files on language-specific elements of programming concepts, or something else. Maintainers are free to organise this directory in the way they feel it best contributes to developing their track.
- **transitions:** Contains a file for each pair of languages, where someone with a background in X learns new language Y. These should explain the concepts that needed to be remapped/learnt. The files in this directory initially aim to help inform us about both the language-agnostic and language-specific files, but will hopefully also in the long-run provide custom pathways for people learning languages.

## GitHub

The following labels are used to categorize [issues in the v3 repository][github-issues]:

- [type/new-exercise][github-issues-type-new-exercise]: new exercises.
- [type/improve-exercise][github-issues-type-improve-exercise]: improving exercises.
- [type/new-reference][github-issues-type-new-reference]: new reference documents.
- [type/improve-reference][github-issues-type-improve-reference]: improving reference documents.
- [status/help-wanted][github-issues-status-help-wanted]: help wanted.
- [status/in-progress][github-issues-status-in-progress]: being worked on.

Each language also has its own label:

- [track/go][github-issues-track-go]: the Go track.
- [track/ruby][github-issues-track-ruby]: the Ruby track.
- Etc.

You can combine labels to find the issues you'd like, e.g. to [list all the issues for the C# for which help is wanted][github-issues-csharp-status-help-wanted].

[docs]: ./README.md
[languages]: ../../languages/README.md
[reference]: ../../reference/README.md
[github-issues]: https://github.com/exercism/v3/issues
[github-issues-type-new-exercise]: https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%3Atype%2Fnew-exercise
[github-issues-type-improve-exercise]: https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%3Atype%2Fimprove-exercise
[github-issues-type-new-reference]: https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%3Atype%2Fnew-reference
[github-issues-type-improve-reference]: https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%3Atype%2Fimprove-reference
[github-issues-status-help-wanted]: https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%status%2Fhelp-wanted
[github-issues-status-in-progress]: https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%status%2Fin-progress
[github-issues-track-go]: https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%3Atrack%2Fgo
[github-issues-track-ruby]: https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%3Atrack%2Fruby
[github-issues-csharp-status-help-wanted]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Fcsharp+
