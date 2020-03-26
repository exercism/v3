This issue describes how to implement the `strings` concept exercise for the F# track.

## Goal

The goal of this exercise is to teach the student the basics of the Concept of Strings in [F#][strings].

## Learning objectives

- The existence of the `string` type.
- How to create a string.
- The existence of `string` methods and `String` module function
- Basic string methods (like finding the index of a character at a position, or returning a part the string).
- Basic string formatting (using `+` and `sprintf`).
- Strings are immutable.

## Out of scope

- Strings can be enumerated.
- String slicing.
- Triple-quoted strings.
- Double-backtick strings.
- Advanced string formatting (combining types).
- Using standard or custom format strings.
- Type inference.
- Memory and performance characteristics.

## Concepts

The Concepts this exercise unlocks are:

- `strings-basic`: know of the existence of the `string` type; know of some basic functions (like looking up a character at a position, or slicing the string); know how to do basic string concatenation.

## Prerequisites

There should be no prerequisites.

## Resources to refer to

### Hints

- [String class][strings]
- [String module][string-module]
- [String concatenation][string-concat]

### After

- [Strings reference](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/strings): includes details on verbatim strings, triple-quoted strings and slicing.

## Representer

This exercise does not require any specific representation logic to be added to the [representer][representer].

## Analyzer

This exercise does not require any specific logic to be added to the [analyzer][analyzer].

## Implementing

To implement the concept exercise, the following files must be created:

<pre>
languages
└── fsharp
    └── exercises
        └── concept
            └── strings
                ├── .docs
                |   ├── instructions.md
                |   ├── introduction.md
                |   ├── hints.md
                |   └── after.md
                ├── .meta
                |   ├── config.json
                |   |── design.md
                |   └── Example.fs
                ├── Strings.fs
                ├── Strings.fsproj
                └── StringsTests.fs
</pre>

## Step 1: add .docs/introduction.md

This file contains an introduction to the concept. It should make the exercise's learning goals explicit and provide a short introduction with enough detail for the student to complete the exercise. The aim is to give the student just enough context to figure out the solution themselves, as research has shown that self-discovery is the most effective learning experience. Using the proper technical terms in the descriptions will be helpful if the student wants to search for more information. If the exercise introduces new syntax, an example of the syntax should always be included; students should not need to search the web for examples of syntax.

As an example, the introduction to a "strings" exercise might describe a string as just a "Sequence of Unicode characters" or a "series of bytes". Unless the student needs to understand more nuanced details in order to solve the exercise, this type of brief explanation (along with an example of its syntax) should be sufficient information for the student to solve the exercise.

## Step 2: add .docs/instructions.md

This file contains instructions for the exercise. It should explicitly explain what the student needs to do (define a method with the signature `X(...)` that takes an A and returns a Z), and provide at least one example usage of that function. If there are multiple tasks within the exercise, it should provide an example of each.

## Step 3: add .docs/hints.md

If the student gets stuck, we will allow them to click a button requesting a hint, which shows this file. This will not be a "recommended" path and we will (softly) discourage them using it unless they can't progress without it. As such, it's worth considering that the student reading it will be a little confused/overwhelmed and maybe frustrated.

The file should contain both general and task-specific "hints". These hints should be enough to unblock almost any student. They might link to the docs of the functions that need to be used.

The hints should not spell out the solution, but instead point to a resource describing the solution (e.g. linking to documentation for the function to use).

## Step 4: add .docs/after.md

Once the student completes the exercise they will be shown this file, which should provide them with a summary of what the exercise aimed to teach. This document can also link to any additional resources that might be interesting to the student in the context of the exercise.

These files are also all described in the [concept exercises document][docs-concept-exercises].

## Step 5: update languages/fsharp/config.json

An entry should be added to the track's `config.json` file for the new concept exercise:

```json
{
  ...
  "exercises": {
    "concept": [
      ...
      {
        "slug": "strings",
        "uuid": "9c2aad8a-53ee-11ea-8d77-2e728ce88125",
        "concepts": ["strings-basic"],
        "prerequisites": []
      }
    ]
  }
}
```

## Step 6: adding track-specific files

These files are specific to the F# track:

- `Strings.fs`. the stub implementation file, which is the starting point for students to work on the exercise.
- `Strings.fsproj`: the F# project file.
- `StringsTests.fs`: the test suite.
- `.meta/Example.fs`: an example implementation that passes all the tests. The project file should _not_ include this file.

## Step 7: update the general concept document

Add the exercise to the [concept's shared document's][reference] `## Implementations` section ([example](https://github.com/exercism/v3/blob/master/reference/types/string.md#implementations)).

## Step 8: updating list of implemented exercises

- Add the exercise to the [list of implemented exercises][implemented-exercises].

## Step 9: add .meta/design.md:

This file contains information on the exercise's design, which includes things like its goal, its teaching goals, what not to teach, and more ([example][meta-design]). This information can be extracted from this GitHub issue.

## Step 10: add .meta/config.json:

This file contains meta information on the exercise, which currently only includes the exercise's contributors ([example][meta-config.json]).

## Step 11: add reference documentation

This exercise could benefit from having a reference document written to introduce basic string concatenation in F#. See [this issue][string-concat-issue]

## Help

If you have any questions while implementing the exercise, please post the questions as comments in this issue.

[how-to-implement-a-concept-exercise]: https://github.com/exercism/v3/blob/master/docs/maintainers/generic-how-to-implement-a-concept-exercise.md
[implemented-exercises]: https://github.com/exercism/v3/tree/master/languages/fsharp/exercises/concept/README.md#implemented-exercises
[reference]: https://github.com/exercism/v3/blob/master/languages/fsharp/reference/README.md#reference-docs
[analyzer]: https://github.com/exercism/fsharp-analyzer
[representer]: https://github.com/exercism/fsharp-representer
[meta-design]: https://github.com/exercism/v3/blob/master/languages/fsharp/exercises/concept/numbers/.meta/design.md
[meta-config.json]: https://github.com/exercism/v3/blob/master/languages/fsharp/exercises/concept/numbers/.meta/config.json
[docs-concept-exercises]: https://github.com/exercism/v3/blob/master/docs/concept-exercises.md
[string-concat-issue]: https://github.com/exercism/v3/issues/757
[strings]: https://docs.microsoft.com/en-us/dotnet/api/system.string?view=netcore-3.1
[string-module]: https://msdn.microsoft.com/visualfsharpdocs/conceptual/core.string-module-%5bfsharp%5d
[string-concat]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/strings#string-operators
