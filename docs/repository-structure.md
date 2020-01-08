# How is this repository structured?

This repository can be view both via GitHub and via the v3 website. This page describes the repository as found on GitHub. It is roughly equivelent to the website, with filenames being titelized.

Things are grouped into several subdirectories:

- **docs:** This directory contains files explaining v3 and how to contribute.
- **reference:** A list of reference documents for the various CS concepts, types, paradigms, etc.
  - **concepts:** This directory contains files discussing **programming** concepts. These documents should provide guidance, limitations, and links for maintainers who need to expound those ideas on their tracks.
  - **tooling** This directory contains documents on tooling that is common across languages (either due to a shared ecosystem - such as JS/TS/Clojurescript - or a shared platform - such as .net or JVM).
  - **types:** This directory contains files discussing programming types. These documents should provide guidance, limitations, and links for maintainers who need to expound those ideas on their tracks.
- **languages:** A directory for each language containing a README.md that outlines the concepts that are necessary to learn to become fluent in those languages. Each language has a config.json and the following subdirectories:
  - **transitions:** Contains a file for each pair of languages, where someone with a background in X learns new language Y. These should explain the concepts that needed to be remapped/learnt. The files in this directory initially aim to help inform us about both the language-agnostic and language-specific files, but will hopefully also in the long-run provide custom pathways for people learning languages.
  - **reference:** Files containing information that the implementer might find useful. These may be files on language-specific elements of programming concepts or notes on the idiomatic way to use tooling. For now, maintainers are encouraged to organise this directory however they feel best, and we may formalise a structure later.
  - **exercises/concept:** The WIP concept exercises for that language.
  - **config.json** The WIP config.json for a track, with the practice exercises removed to avoid noise.
