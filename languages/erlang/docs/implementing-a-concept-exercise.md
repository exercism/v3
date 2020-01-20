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
                |   └── example.erl
                ├── src
                |   ├── <NAME>.app.src
                |   └── <NAME>.erl
                ├── test
                |   └── <NAME>_tests.erl
                └── rebar.config
```

[concept-exercises]: ../exercises/concept/README.md
