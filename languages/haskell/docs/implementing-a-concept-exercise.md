# How to implement a Haskell concept exercise

This document describes the steps required to implement a concept exercise for the Haskell track. As this document is generic, the following placeholders are used:

- `<SLUG>`: the name of the exercise in kebab-case (e.g. `anonymous-methods`).
- `<NAME>`: the name of the exercise in PascalCase (e.g. `AnonymousMethods`).

Before implementing the exercise, please make sure you have a good understanding of what the exercise should be teaching (and what not). This information can be found in the exercise's GitHub issue.

To implement a concept exercise,  the following files need to be created:

<pre>
languages
└── Haskell
    └── exercises
        └── concept
            └── <SLUG>
                ├── .docs
                |   ├── instructions.md
                |   ├── introduction.md
                |   ├── hints.md
                |   └── after.md (optional)
                ├── .meta
                |   ├── config.json
                |   ├── <SLUG>.cabal
                |   ├── stack.yaml
                |   └── Example.hs
                ├── src
                |   └── <NAME>.hs
                └── test
                    └── Main.hs
</pre>

## Step 1: add track-specific files

These are files specific to the PureScript track:
- `src/<NAME>.hs`: the stub implementation file, which is the starting point for students to work on the exercise.
- `test/Main.hs`: the test suite.
- `stack.yaml`: the stack project file.
- `.meta/Example.hs`: an example implementation that passes all the tests.

## Step 2: add common files

How to create the files common to all tracks is described in the [how to implement a concept exercise document][how-to-implement-a-concept-exercise].

## Inspiration

When implementing an exercise, it can be very useful to look at already implemented Haskell exercises. You can also check the exercise's [general concepts documents][reference] to see if other languages have already implemented an exercise for that concept.

[reference]: ../../../reference
[how-to-implement-a-concept-exercise]: ../../../docs/maintainers/generic-how-to-implement-a-concept-exercise.md
