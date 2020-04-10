# How to implement a Clojure concept exercise

This document describes how to implement a concept exercise for the Clojure track.

**Please please please read the docs before starting.** Posting PRs without reading these docs will be a lot more frustrating for you during the review cycle, and exhaust Exercism's maintainers' time. So, before diving into the implementation, please read up on the following documents:

- [The features of v3][docs-features-of-v3].
- [Rationale for v3][docs-rationale-for-v3].
- [What are concept exercise and how they are structured?][docs-concept-exercises]

As this document is generic, the following placeholders are used:

- `<SLUG>`: the name of the exercise in kebab-case (e.g. `anonymous-methods`).
- `<NAME>`: the name of the exercise in PascalCase (e.g. `AnonymousMethods`).

Before implementing the exercise, please make sure you have a good understanding of what the exercise should be teaching (and what not). This information can be found in the exercise's GitHub issue. Having done this, please read the [Clojure concept exercises introduction]//TODO.

To implement a concept exercise, the following files must be created:

<pre>
languages
└── clojure
    └── exercises
        └── concept
            └── &lt;SLUG&gt;
                ├── .docs
                |   ├── instructions.md
                |   ├── introduction.md
                |   ├── hints.md
                |   ├── source.md (required if there are third-party sources)
                |   └── after.md
                ├── .meta
                |   |── config.json
                |   |── design.md
                |   └── Example.clj
                ├── &lt;NAME&gt;.clj
                └── &lt;NAME&gt;_test.clj
</pre>

## Step 1: adding track-specific files

These files are specific to the Clojure track:

- `<NAME>.clj`: the Clojure file.
- `<NAME>_test.clj`: the test suite.

## Step 2: adding common files

How to create the files common to all tracks is described in the [how to implement a concept exercise document][how-to-implement-a-concept-exercise].

## Step 3: add analyzer (optional)

//TODO

## Step 4: custom representation (optional)

//TODO

## Inspiration

//TODO

## Help

If you have any questions regarding implementing the exercise, please post them as comments in the exercise's GitHub issue.

//To-Add-More

[how-to-implement-a-concept-exercise]: ../../../docs/maintainers/generic-how-to-implement-a-concept-exercise.md
[docs-concept-exercises]: ../../../docs/concept-exercises.md
[docs-rationale-for-v3]: ../../../docs/rationale-for-v3.md
[docs-features-of-v3]: ../../../docs/features-of-v3.md
