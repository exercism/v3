# How to implement an R concept exercise

This document describes how to implement a concept exercise for the R track.

**Please please please read the docs before starting.** Posting PRs without reading these docs will be a lot more frustrating for you during the review cycle, and exhaust Exercism's maintainers' time. So, before diving into the implementation, please read the following documents:

- [The features of v3][docs-features-of-v3].
- [Rationale for v3][docs-rationale-for-v3].
- [What are concept exercises and how are they structured?][docs-concept-exercises]

Please also watch the following video:

- [The Anatomy of a Concept Exercise][anatomy-of-a-concept-exercise].

As this document is generic, the following placeholders are used:

- `<SLUG>`: the slug of the exercise in kebab-case (e.g. `calculator-conundrum`).
- `<NAME>`: the name of the exercise in PascalCase (e.g. `CalculatorConundrum`).
- `<CONCEPT_SLUG>`: the slug of one of the exercise's concepts in kebab-case (e.g. `anonymous-methods`).

Before implementing the exercise, please make sure you have a good understanding of what the exercise should be teaching (and what not). This information can be found in the exercise's GitHub issue.

To implement a concept exercise, the following files must be added:

<pre>
languages
└── r
    ├── concepts
    |   └── <CONCEPT_SLUG>
    |       ├── about.md
    |       ├── introduction.md
    |       └── links.json
    └── exercises
        └── concept
            └── <SLUG>
                ├── .docs
                |   ├── instructions.md
                |   ├── introduction.md
                |   ├── hints.md
                |   └── source.md (required if there are third-party sources)
                ├── .meta
                |   |── config.json
                |   |── design.md
                |   └── example.R
                ├── <NAME>.R
                └── <NAME>_test.R
</pre>

## Step 1: Add code files

The code files are track-specific and should be designed to help the student learn the exercise's concepts. The following R code files must be added (not necessarily in this order):

- `<NAME>.R`. the stub implementation file, which is the starting point for students to work on the exercise.
- `<NAME>_test.R`: the test suite.
- `.meta/example.R`: an example implementation that passes all the tests.

## Step 2: Add documentation files

How to create the files common to all tracks is described in the [how to implement a concept exercise document][how-to-implement-a-concept-exercise].

## Inspiration

When implementing an exercise, it can be very useful to look at already implemented R exercises. You can also check the exercise's [general concepts documents][reference] to see if other languages have already implemented an exercise for that concept.

## Help

If you have any questions regarding implementing the exercise, please post them as comments in the exercise's GitHub issue.

[how-to-implement-a-concept-exercise]: ../../../docs/maintainers/generic-how-to-implement-a-concept-exercise.md
[docs-concept-exercises]: ../../../docs/concept-exercises.md
[docs-rationale-for-v3]: ../../../docs/rationale-for-v3.md
[docs-features-of-v3]: ../../../docs/features-of-v3.md
[anatomy-of-a-concept-exercise]: https://www.youtube.com/watch?v=gkbBqd7hPrA
[reference]: ../../../reference
