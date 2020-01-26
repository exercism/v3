# How to implement an F# concept exercise

This document describes how to implement a concept exercise for the F# track. As this document is generic, the following placeholders are used:

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
                |   └── after.md (optional)
                ├── .meta
                |   └── Example.fs
                ├── &lt;NAME&gt;.fs
                ├── &lt;NAME&gt;.fsproj
                └── &lt;NAME&gt;Test.fs
</pre>

## Step 1: adding track-specific files

These files are specific to the F# track:

- `<NAME>.fs`. the stub implementation file, which is the starting point for students to work on the exercise.
- `<NAME>.fsproj`: the F# project file.
- `<NAME>Test.fs`: the test suite.
- `.meta/Example.fs`: an example implementation that passes all the tests. The project file should _not_ include this file.

## Step 2: adding common files

How to create the files common to all tracks is described in the [how to implement a concept exercise document][how-to-implement-a-concept-exercise].

## Inspiration

When implementing an exericse, it can be very useful to check the exercise's [general concepts documents][reference] to see if other languages that have already implemented an exercise for that concept.

## Help

If you have any questions regarding implementing this exercise, please post them as comments in the exercise's GitHub issue.

[concept-exercises]: ../exercises/concept/README.md
[how-to-implement-a-concept-exercise]: ../../../docs/maintainers/generic-how-to-implement-a-concept-exercise.md
[reference]: ../../../reference
