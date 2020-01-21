# How to implement an Erlang concept exercise

This document describes how to implement a concept exercise for the Erlang track.
As this document is generic, the following placeholders are used:

- `<SLUG>`: the name of the exercise in kebab-case (e.g. `anonymous-functions`).
- `<NAME>`: the name of the exercise in snake_case (e.g. `anonymous_functions`).

Before implementing the exercise, please make sure you have a good understanding of what the exercise should be teaching (and what not).
This information can be found in the exercise's GitHub issue.
Having done this, please read the [Erlang concept exercises introduction][concept-exercises].

```plain
languages
└── erlang
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
                |   └── <NAME>.erl
                ├── src
                |   ├── <NAME>.app.src
                |   └── <NAME>.erl
                ├── test
                |   └── <NAME>_tests.erl
                └── rebar.config
```

## Step 1: adding track-specific files

These files are specific to the Erlang track:

- `src/<NAME>.erl`: the stub implementation file, which is the starting point for students to work on the exercise.
- `src/<NAME>.app.src`: the applicatoin definition which is pretty much the same across all exercises except for the application name.
- `test/<NAME>_tests.erl`: the test suite, written using `eunit`.
- `rebar.config`: the `rebar3` project configuration which is usually the same across all exercises.
  It is important that it does not contain any dependencies.
- `.meta/<NAME>.erl`: an example implementation that passes all the tests.

## Step 2: adding common files

How to create the files common to all tracks is described in the [how to implement a concept exercise document][how-to-implement-a-concept-exercise].

## Step 3: add analyzer (optional)

Some exercises could benefit from having an exercise-specific [analyzer][analyzer]. If so, specify what analysis rules should be applied to this exercise and why.

<!-- Lexicographically ordered list of link targets -->

[analyzer]: https://github.com/exercism/erlang-analyzer
[concept-exercises]: ../exercises/concept/README.md
[how-to-implement-a-concept-exercise]: ../../../docs/maintainers/generic-how-to-implement-a-concept-exercise.md
