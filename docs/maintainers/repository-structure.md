# How is this repository structured?

This repository can be viewed both via GitHub and via the v3 website. This page describes the repository as found on GitHub. It is roughly equivalent to the website, with filenames being title-ized.

Things are grouped into several sub-directories:

## [Docs][docs]

This directory contains files, for both the public and maintainers, explaining what v3 is and how to contribute.

## [Reference][reference]

A list of reference documents for the various CS concepts, types, paradigms, etc.

- **concepts:** This directory contains files discussing **programming** concepts. These documents should provide guidance, limitations, and links for maintainers who need to expound those ideas on their tracks.
- **paradigms** This directory contains files discussing programming paradigms. These documents should link to the concepts typically associated with the paradigms.
- **tooling** This directory contains documents on tooling that is common across languages (either due to a shared ecosystem - such as JS/TS/Clojurescript - or a shared platform - such as .net or JVM).
- **types:** This directory contains files discussing programming types. These documents should provide guidance, limitations, and links for maintainers who need to expound those ideas on their tracks.

## [Languages][languages]

Each track has its own directory containing the following files and directories:

- **concepts:** This directory contains files to describe the concepts used in the track.
- **config.json** The updated config.json for a track, with the practice exercises removed to avoid noise.
- **docs:** This directory contains files, for both the public and maintainers, describing the track and how to contribute. It is envisioned that these documents will be linked to from issues and discussions to provide canonical answers or guidance.
- **exercises:** The exercises for that track.
  - **shared:** Exercise files shared across all exercises for that track.
    - **.docs:** Exercise documentation shared across all exercises for that track.
  - **concept:** The Concept Exercises for that track.
  - **practice:** The Practice Exercises for that track.
- **README.md:** The track's README, which should contain a short description of the goal of the repository as well as the status of the track's v3 transition. A template is provided during the bootstrapping process.
- **reference:** Files that help maintainers develop the track. These could be reference documents that help explain track-specific concepts for which no appropriate online document could be found, files on language-specific elements of programming concepts, or something else. Maintainers are free to organise this directory in the way they feel it best contributes to developing their track.
  - **README.md:** The README outlines the concepts that are necessary to learn to become fluent in that language. It could also list any reference documents that have been written.
  - **concepts.csv:** A machine readable file defining all concept slugs used in the track. Read the [Continuous Integration][concept-ci] document for more information. This file is optional.
- **transitions:** Contains a file for each pair of languages, where someone with a background in X learns new language Y. These should explain the concepts that needed to be remapped/learnt. The files in this directory initially aim to help inform us about both the language-agnostic and language-specific files, but will hopefully also in the long-run provide custom pathways for people learning languages.

## GitHub

The following labels are used to categorize [issues in the v3 repository][github-issues]:

- [type/new-exercise][github-issues-type-new-exercise]: Add a new exercise
- [type/improve-exercise][github-issues-type-improve-exercise]: Improve an existing exercise
- [type/suggested-exercise][github-issues-type-suggested-exercise]: Suggest a new (Concept) exercise
- [type/new-reference-doc][github-issues-type-new-reference-doc]: Add a new reference document
- [type/improve-reference-doc][github-issues-type-improve-reference-doc]: Improve an existing reference document
- [type/new-student-facing-doc][github-issues-type-new-student-facing-doc]: Add a new student-facing document
- [type/improve-student-facing-doc][github-issues-type-improve-student-facing-doc]: Improve an existing student-facing document
- [type/launch-checklist][github-issues-type-launch-checklist]: Things that need to be done once a track is sufficiently close to launch
- [type/ci][github-issues-type-ci]: Everything related to Continuous Integration.
- [type/discussion][github-issues-type-discussion]: Discussion regarding v3.
- [type/documentation][github-issues-type-documentation]: Improvements or additions to documentation.
- [type/analyzer-extension][github-issues-type-analyzer-extension]: Add analyzer support for a new (Concept) exercise.
- [status/help-wanted][github-issues-status-help-wanted]: Extra attention is needed
- [status/in-progress][github-issues-status-in-progress]: This issue is being worked on
- [status/draft][github-issues-status-draft]: This issue or pull request is a draft, and not ready for review/merge.
- [status/wontfix][github-issues-status-wontfix]: This issue or pull request is out of scope and will not be fixed.
- [about/v3-roadmap][github-issues-type-v3-roadmap]: Everything related to the v3 roadmap.
- [good-first-issue][github-issues-type-good-first-issue]: Marks an issue as good for newcomers.

Each language also has its own label:

- [track/bash][track-bash]
- [track/c][track-c]
- [track/csharp][track-csharp]
- [track/cpp][track-cpp]
- [track/clojure][track-clojure]
- [track/coffeescript][track-coffeescript]
- [track/common-lisp][track-common-lisp]
- [track/dart][track-dart]
- [track/delphi][track-delphi]
- [track/elixir][track-elixir]
- [track/emacs-lisp][track-emacs-lisp]
- [track/erlang][track-erlang]
- [track/fsharp][track-fsharp]
- [track/factor][track-factor]
- [track/go][track-go]
- [track/haskell][track-haskell]
- [track/java][track-java]
- [track/javascript][track-javascript]
- [track/julia][track-julia]
- [track/kotlin][track-kotlin]
- [track/ocaml][track-ocaml]
- [track/purescript][track-purescript]
- [track/python][track-python]
- [track/reasonml][track-reasonml]
- [track/ruby][track-ruby]
- [track/rust][track-rust]
- [track/scala][track-scala]
- [track/scheme][track-scheme]
- [track/typescript][track-typescript]
- [track/x86-64-assembly][track-x86-64-assembly]

You can combine labels to find the issues you'd like, e.g. to [list all the issues for the C# for which help is wanted][github-issues-csharp-status-help-wanted].

[docs]: ./README.md
[languages]: ../../languages/README.md
[reference]: ../../reference/README.md
[concept-ci]: ./continuous-integration.md#concept-ci
[github-issues]: https://github.com/exercism/v3/issues
[github-issues-type-new-exercise]: https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%3Atype%2Fnew-exercise
[github-issues-type-improve-exercise]: https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%3Atype%2Fimprove-exercise
[github-issues-type-suggested-exercise]: https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%3Atype%2Fsuggested-exercise
[github-issues-type-new-reference-doc]: https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%3Atype%2Fnew-reference-doc
[github-issues-type-improve-reference-doc]: https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%3Atype%2Fimprove-reference-doc
[github-issues-type-new-student-facing-doc]: https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%3Atype%2Fnew-student-facing-doc
[github-issues-type-improve-student-facing-doc]: https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%3Atype%2Fimprove-student-facing-doc
[github-issues-type-launch-checklist]: https://github.com/exercism/v3/labels/type%2Flaunch-checklist
[github-issues-type-ci]: https://github.com/exercism/v3/labels/type%2Fci
[github-issues-type-discussion]: https://github.com/exercism/v3/labels/type%2Fdiscussion
[github-issues-type-documentation]: https://github.com/exercism/v3/labels/type%2Fdocumentation
[github-issues-type-analyzer-extension]: https://github.com/exercism/v3/labels/type%2Fanalyzer-extension
[github-issues-status-help-wanted]: https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted
[github-issues-status-in-progress]: https://github.com/exercism/v3/issues?q=is%3Aopen+is%3Aissue+label%3Astatus%2Fin-progress
[github-issues-status-draft]: https://github.com/exercism/v3/issues?q=is%3Aopen+is%3Aissue+label%3Astatus%2Fdraft
[github-issues-status-wontfix]: https://github.com/exercism/v3/issues?q=is%3Aissue+label%3Astatus%2Fwontfix+is%3Aclosed
[github-issues-type-v3-roadmap]: https://github.com/exercism/v3/labels/about%2Fv3-roadmap
[github-issues-type-good-first-issue]: https://github.com/exercism/v3/labels/good%20first%20issue
[github-issues-track-go]: https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%3Atrack%2Fgo
[github-issues-track-ruby]: https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%3Atrack%2Fruby
[github-issues-csharp-status-help-wanted]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Fcsharp+
[track-bash]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Fbash
[track-c]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Fc
[track-csharp]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Fcsharp
[track-cpp]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Fcpp
[track-clojure]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Fclojure
[track-coffeescript]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Fcoffeescript
[track-common-lisp]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Fcommon-lisp
[track-dart]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Fdart
[track-delphi]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Fdelphi
[track-elixir]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Felixir
[track-emacs-lisp]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Femac-lisp
[track-erlang]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Ferland
[track-fsharp]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Ffsharp
[track-factor]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Ffactor
[track-go]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Fgo
[track-haskell]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Fhaskell
[track-java]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Fjava
[track-javascript]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Fjavascript
[track-julia]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Fjulia
[track-kotlin]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Fkotlin
[track-ocaml]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Focaml
[track-purescript]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Fpurescript
[track-python]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Fpython
[track-reasonml]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Freasonml
[track-ruby]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Fruby
[track-rust]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Frust
[track-scala]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Fscala
[track-scheme]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Fscheme
[track-typescript]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Ftypescript
[track-x86-64-assembly]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Astatus%2Fhelp-wanted+label%3Atrack%2Fx86-64-assembly
