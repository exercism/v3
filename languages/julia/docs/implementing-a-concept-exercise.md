# How to implement a Julia concept exercise

This document describes how to implement a concept exercise for the Julia track. As this document is generic, the following placeholders are used:

- `$slug`: the name of the exercise in kebab-case (e.g. `anonymous-methods`).

Before implementing the exercise, please make sure you have a good understanding of what the exercise should be teaching (and what not). This information can be found in the exercise's GitHub issue. Having done this, please read the [Julia concept exercises introduction][concept-exercises].

To implement a concept exercise, the following files must be created:

```
languages
└── julia
    └── exercises
        └── concept
            └── $slug
                ├── .docs
                |   ├── instructions.md
                |   ├── introduction.md
                |   ├── hints.md
                |   └── after.md (optional)
                ├── .meta
                |   └── example.jl
                ├── $slug.jl
                ├── $runtests.jl
                ├── Project.toml (optional)
                └── Manifest.toml (optional)
```

## Step 1: adding track-specific files

These files are specific to the Julia track:

- `$slug.jl`: the stub implementation file, which is the starting point for students to work on the exercise.
- `runtests.jl`: the test suite.
- `.meta/example.jl`: an example implementation that passes all the tests.
- Project.toml (optional): if dependencies are required, provide this file for a reproducible environment.
- Manifest.toml (optional): if dependencies are required, provide this file for a reproducible environment.

## Step 2: adding common files

How to create the files common to all tracks is described in the [how to implement a concept exercise document][how-to-implement-a-concept-exercise].

## Step 3: adding the exercise to the general concept document

Add the exercise to the [concept's shared document's][reference] `## Implementations` section.

## Inspiration

When implementing an exercise, it can be very useful to look at already implemented Julia exercises like the [multiple-dispatch][concept-exercise-multiple-dispatch] exercise. You can also check the exercise's [general concepts documents][reference] to see if other languages have already implemented an exercise for that concept.

## Help

If you have any questions regarding implementing the exercise, please post them as comments in the exercise's GitHub issue.

[concept-exercises]: ../exercises/concept/README.md
[how-to-implement-a-concept-exercise]: ../../../docs/maintainers/generic-how-to-implement-a-concept-exercise.md
[concept-exercise-strings]: ../exercises/concept/multiple-dispatch
[reference]: ../../../reference
