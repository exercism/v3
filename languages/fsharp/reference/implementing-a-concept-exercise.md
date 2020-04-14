# How to implement an F# concept exercise

This document describes how to implement a concept exercise for the F# track.

**Please please please read the docs before starting.** Posting PRs without reading these docs will be a lot more frustrating for you during the review cycle, and exhaust Exercism's maintainers' time. So, before diving into the implementation, please read the following documents:

- [The features of v3][docs-features-of-v3].
- [Rationale for v3][docs-rationale-for-v3].
- [What are concept exercise and how they are structured?][docs-concept-exercises]

Please also watch the following video:

- [The Anatomy of a Concept Exercise][anatomy-of-a-concept-exercise].

As this document is generic, the following placeholders are used:

- `<SLUG>`: the name of the exercise in kebab-case (e.g. `anonymous-methods`).
- `<NAME>`: the name of the exercise in PascalCase (e.g. `AnonymousMethods`).

Before implementing the exercise, please make sure you have a good understanding of what the exercise should be teaching (and what not). This information can be found in the exercise's GitHub issue. Having done this, please read the [F# concept exercises introduction][concept-exercises].

To implement a concept exercise, the following files must be created:

<pre>
languages
└── fsharp
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
                |   └── Example.fs
                ├── &lt;NAME&gt;.fs
                ├── &lt;NAME&gt;.fsproj
                └── &lt;NAME&gt;Tests.fs
</pre>

## Step 1: adding track-specific files

These files are specific to the F# track:

- `<NAME>.fs`. the stub implementation file, which is the starting point for students to work on the exercise.
- `<NAME>.fsproj`: the F# project file.
- `<NAME>Tests.fs`: the test suite.
- `.meta/Example.fs`: an example implementation that passes all the tests. The project file should _not_ include this file.

## Step 2: adding common files

How to create the files common to all tracks is described in the [how to implement a concept exercise document][how-to-implement-a-concept-exercise].

## Inspiration

When implementing an exericse, it can be very useful to check the exercise's [general concepts documents][reference] to see if other languages that have already implemented an exercise for that concept.

## Help

If you have any questions regarding implementing this exercise, please post them as comments in the exercise's GitHub issue.

[concept-exercises]: ../exercises/concept/README.md
[how-to-implement-a-concept-exercise]: ../../../docs/maintainers/generic-how-to-implement-a-concept-exercise.md
[docs-concept-exercises]: ../../../docs/concept-exercises.md
[docs-rationale-for-v3]: ../../../docs/rationale-for-v3.md
[docs-features-of-v3]: ../../../docs/features-of-v3.md
[anatomy-of-a-concept-exercise]: https://www.youtube.com/watch?v=gkbBqd7hPrA
[reference]: ../../../reference
