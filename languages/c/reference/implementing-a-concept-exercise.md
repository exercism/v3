# How to Implement a C Concept Exercise

This document describes how to implement a concept exercise for the C track. As this document is generic, the following placeholders are used:

- `<SLUG>`: the name of the exercise in kebab-case (e.g. `function-pointers`).
- `<NAME>`: the name of the exercise in snake_case (e.g. `function_pointers`).

Before implementing the exercise, please make sure you have a good understanding  of what the exercise should be teaching (and what not). This information can be found in the exercise's GitHub issue.

To implement a concept exercise, the following files must be created:
<pre>
languages
└── c
    └── exercises
        └── concept
            └── &lt;SLUG&gt;
                ├── .docs
                │   ├── instructions.md
                │   ├── introduction.md
                │   ├── hints.md
                │   └── after.md (optional)
                ├── .meta
                │   ├── config.json
                │   ├── design.md
                │   ├── example.c
                │   └── example.c
                ├── src
                │   ├── &lt;NAME&gt;.c
                │   └── &lt;NAME&gt;.h
                ├── test
                │   ├── vendor
                │   └── test_&lt;NAME&gt;.c
                └── makefile
</pre>

## Step 1: Adding Track-Specific Files

These files are specific to the C track:

- `src/<NAME>{.c|.h}`: the stub source and header implementation files, which are the starting point for students to work on the exercise.
- `test/test_<NAME>.c`: the unit test source file.
- `.meta/example{.c|.h}`:  an example implementation that passes all the unit tests.
- `test/vendor`: this directory contains the unit test framework, [Unity][unity]

## Step 2: Adding Common Files

How to create the files common to all tracks is described in the [how to implement a concept exercise document][how-to-implement-a-concept-exercise].

## Inspiration

When implementing an exercise, it can be very useful to look at already implemented [C exercises][exercises]. You can also check the exercise's [general concepts documents][reference] to see if other languages have already implemented an exercise for that concept.

## Help

If you have any questions regarding implementing the exercise, please post them as comments in the exercise's GitHub issue.

[unity]: http://www.throwtheswitch.org/unity
[how-to-implement-a-concept-exercise]: ../../../docs/maintainers/generic-how-to-implement-a-concept-exercise.md
[exercises]: ../exercises
[reference]: ../../../reference