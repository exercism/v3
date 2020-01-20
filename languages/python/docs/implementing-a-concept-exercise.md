# How to implement a Python concept exercise

This document describes the steps required to implement a concept exercise for the Python track. As this document is generic, the following placeholders are used:

- `<SLUG>`: the name of the exercise in kebab-case (e.g. `anonymous-methods`).
- `<NAME>`: the name of the exercise in PascalCase (e.g. `AnonymousMethods`).

Before implementing the exercise, please make sure you have a good understanding of what the exercise should be teaching (and what not). This information can be found in the exercise's GitHub issue.

To implement a concept exercise, the following files need to be created:

```
languages
└── Python
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
                |   └── <SLUG>.py
                ├── src
                |   └── <NAME>.py
                └── test
                    └── Main.py

```

## Step 1: add track-specific files

These are files specific to the Python track:
- `src/<NAME>.py`: the stub implementation file, which is the starting point for students to work on the exercise.
- `test/Main.py`: the test suite.
- `.meta/Example.py`: an example implementation that passes all the tests.

## Step 2: add common files

How to create the files common to all tracks is described in the [how to implement a concept exercise document][how-to-implement-a-concept-exercise].

## Inspiration

When implementing an exercise, it can be very useful to look at already implemented Python exercises. You can also check the exercise's [general concepts documents][reference] to see if other languages have already implemented an exercise for that concept.

[reference]: ../../../reference
[how-to-implement-a-concept-exercise]: ../../../docs/maintainers/generic-how-to-implement-a-concept-exercise.md
