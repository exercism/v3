# Design

## Goal

The goal of this exercise is to introduce the student to C# properties and to teach the student how the concept of properties is implemented in [C#][docs.microsoft.com-properties]. We'll teach the student about properties by having the student work with classes that expose contrasting forms of properties. The students will learn to work with properties with backing fields and auto-implemented properties.

Properties are covered early in the C# track as their purpose and power can be shown with few dependencies (classes, access modifiers and fields of simple types).


## Learning objectives

- Know what properties are and how they relate to fields and methods.
- Know what backing-field properties are.
- Know what auto-implemented properties are.
- Know what calculated properties are.
- Know how to use property accessors to customize visibility.
- Know how to define the different types of properties.

There is no way to enforce the use of backing fields in preference to auto-implemented properties
or vice versa either through tests or by giving a steer in the scenario.  The best we can hope for
is that mentors' comments offer some opinion and the alternative.

Similarly there is no way to distinguish between set/get-only properties and those with
private accessors. 


## Out of scope

- expression bodied properties, get accessors and set accessors (covered by expression-bodied members)
- properties on interfaces (covered by Interfaces)
- properties/absract properties on abstract classes (covered by Inheritance)
- use of the `readonly` keyword with properties (covered by Immutability)
- static properties (covered by Statics)
- indexers (covered by Indexers)

Note that students may choose to implement expression-bodied members.

## Concepts

This Concepts Exercise's concepts are:

Construction of an expressive API with fine-grained control over access.

## Prequisites

- `numbers-basic`: value types such as int and float
- `classes-basic`: defining classes and working with members.
* structs
* Multiple classes per file
* number casting, rounding and truncation

Note that the values in the instructions' examples and tests
are selected to avoid any question of rounding when converting
between float and int.  Rounding and truncation will produce the 
same result.

Prerequisite Exercises - TBA

## Resources to refer to

### Hints

Examples:
- [Properties][docs.microsoft.com-properties]
- [Using Properties][docs.microsoft.com-using-properties]

### After

Examples:
- [Properties][docs.microsoft.com-properties]
- [Using Properties][docs.microsoft.com-using-properties]

As this is an introductory exercise, we should take care not to link to very advanced resources, to prevent overwhelming the student.

## Representer

TBC

## Analyzer

TBC

## Implementing

If you'd like to work on implementing this exercise, the first step is to let us know through a comment on this issue, to prevent multiple people from working on the same exercise. If you have any questions while implementing the exercise, please also post them as comments in this issue.

Implementing the exercise means creating the following files:

<pre>
languages
└── csharp
    └── exercises
        └── concept
            └── properties
                ├── .docs
                |   ├── after.md
                |   ├── hints.md
                |   ├── instructions.md
                |   └── introduction.md
                ├── .meta
                |   ├── design.md
                |   └── Example.cs
                ├── Properties.csproj
                ├── Properties.cs
                └── PropertiesTest.cs
</pre>

## Step 1: add .docs/introduction.md

This file contains an introduction to the concept. It should be explicit about what the exercise teaches and maybe provide a brief introduction to the concepts, but not give away so much that the user doesn't have to do any work to solve the exercise.

## Step 2: add .docs/instructions.md

This file contains instructions for the exercise. It should explicitly explain what the user needs to do (define a method with the signature `X(...)` that takes an A and returns a Z), and provide at least one example usage of that function. If there are multiple tasks within the exercise, it should provide an example of each.

## Step 3: add .docs/hints.md

If the user gets stuck, we will allow them to click a button requesting a hint, which shows this file. We will softly discourage them using it. The file should contain both general and task-specific "hints". These hints should be enough to unblock almost any student.

## Step 4: add .docs/after.md

Once the user completes the exercise they will be shown this file, which gives them any bonus information or further reading about the concept taught.

These files are also all described in the [concept exercises document][docs-concept-exercises].

## Step 5: update languages/csharp/config.json

An entry should be added to the track's `config.json` file for the new concept exercise:

```json
{
  ...
  "exercises": {
    "concept": [
      ...
      {
        "slug": "properties",
        "uuid": "xxx+xxxx….",
        "concepts": [“properties”],
        "prerequisites": ["TBC"]
      }
    ]
  }
}
```

## Step 6: adding track-specific files

These files are specific to the C# track:

- `Properties.csproj`: the C# project file.
- `PropertiesTest.cs`: the test suite.
- `Properties.cs`. the stub implementation file, which is the starting point for students to work on the exercise.
- `.meta/Example.cs`: an example implementation that passes all the tests.

Check out the [`floating-point-numbers exercise`][csharp-docs-concept-exercises-floating-point-numbers] for an example on what these files should look like.

## Step 7: update the general concept document

Not applicable for this concept

## Step 8: updating list of implemented exercises

- Add the exercise to the [list of implemented exercises][csharp-docs-concept-exercises].

## Step 9: add .meta/design.md:

This file contains information on the exercise's design, which includes things like its goal, its teaching goals, what not to teach, and more ([example][meta-design]). This information can be extracted from this GitHub issue.

### Inspiration

When implementing this exericse, it can be very useful to look at already implemented C# exercises like the [strings][csharp-docs-concept-exercises-strings], [dates][csharp-docs-concept-exercises-dates] or [floating-point numbers][csharp-docs-concept-exercises-floating-point-numbers] exercises.

[docs.microsoft.com-properties]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/properties
[docs.microsoft.com-using-properties]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/using-properties
[docs.microsoft.com-foreach-with-arrays]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/arrays/using-foreach-with-arrays
[docs.microsoft.com-single-dimensional-arrays]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/arrays/single-dimensional-arrays
[docs.microsoft.com-implicitly-typed-arrays]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/arrays/implicitly-typed-arrays
[docs-v3]: https://github.com/exercism/v3/blob/master/docs/concept-exercises.md#exercise-structure
[docs-v3-types-array]: https://github.com/exercism/v3/blob/master/reference/types/array.md
[docs-v3-types-collection]: https://github.com/exercism/v3/blob/master/reference/types/collection.md
[csharp-docs]: https://github.com/exercism/v3/blob/master/languages/csharp/README.md
[csharp-docs-concept-exercises-strings]: https://github.com/exercism/v3/tree/master/languages/csharp/exercises/concept/strings
[csharp-docs-concept-exercises-dates]: https://github.com/exercism/v3/tree/master/languages/csharp/exercises/concept/dates
[csharp-docs-concept-exercises-floating-point-numbers]: https://github.com/exercism/v3/tree/master/languages/csharp/exercises/concept/numbers-floating-point
[csharp-analyzer]: https://github.com/exercism/csharp-analyzer
[csharp-representer]: https://github.com/exercism/csharp-representer
[csharp-docs-cli.md]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/.docs/cli.md
[csharp-docs-debug.md]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/.docs/debug.md
[csharp-docs-after.md]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/numbers-floating-point/.docs/after.md
[csharp-docs-hints.md]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/numbers-floating-point/.docs/hints.md
[csharp-docs-introduction.md]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/numbers-floating-point/.docs/introduction.md
[csharp-docs-instructions.md]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/numbers-floating-point/.docs/instructions.md
[csharp-docs-design.md]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/numbers-floating-point/.docs/design.md
[csharp-meta-config.json]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/numbers-floating-point/.meta/config.json
[csharp-docs-concept-exercises]: https://github.com/exercism/v3/tree/master/languages/csharp/exercises/concept/README.md
[referrence-array]: https://github.com/exercism/v3/blob/master/reference/types/array.md
