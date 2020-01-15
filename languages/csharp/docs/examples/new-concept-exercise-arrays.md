# [C#] Add new Concept Exercise - arrays

This issue describes a new `arrays` exercise that should be added to the [v3 C# track][csharp-docs].

## Goal

The goal of this exercise is to teach the student how the concept of [collections][docs-v3-types-collection] is implemented in [C#][docs.microsoft.com-collections]. We'll teach the student about collections by having the student work with one specific type of collection, namely the [array][docs-v3-types-array]. The students will learn to define arrays, iterate over array items, access items by index, and more.

Of the many available C# collection types, we chose to use the `array` collection type as the first collection type students will be taught for the following reasons:

- Arrays don't require the student to know about generics.
- Arrays are a common data type in many language.
- Arrays have a fixed length. No complexity in adding or removing elements.
- Arrays have a simple shorthand syntax. No need to understand how constructors work to define an array.

## Things to teach

After completing this exercise, the student should:

- Know of the existence of the `Array` type.
- Know how to define an array.
- Know how to access elements in an array by index.
- Know how to iterate over elements in an array.
- Know of some basic array functions (like finding the index of an element in an array).
- Know where it's documented, or at least how to search for it.

## Things not to teach

The following things are outside the scope of this exercise:

- Multi-dimensional/jagged arrays.
- Memory and performance characteristics of arrays.
- Enumerables.
- Iterators.
- LINQ.

## Resources to refer to

Here are some suggestions for resources to use in the exercise's documentation file(s):

### Hints

- [Arrays][docs.microsoft.com-arrays]
- [Single-dimensional arrays][docs.microsoft.com-single-dimensional-arrays]
- [Usings foreach with arrays][docs.microsoft.com-foreach-with-arrays]

### After

- [Collections][docs.microsoft.com-collections]
- [Implicitly typed arrays][docs.microsoft.com-implicitly-typed-arrays]

As this is an introductory exercise, we should take care not to link to very advanced resources, to prevent overwhelming the student.

## Concepts

This Concepts Exercise's Concepts are:

- `basic-collections`
- `basic-arrays`

## Prequisites

As an array is a collection type, it holds zero or more instances of another type. That means it _has_ to depend on one or more other types. In this exercise, we'll use the `int` data type for that, which is both interesting enough and easy to work with. The `int` data type is introduced in the `basic-numbers` concept.

This Concept Exercise's prerequisites Concepts are:

- `basic-numbers`

## Implementing

If you'd like to work on implementing this exercise, the first step is to let us know through a comment on this issue, to prevent multiple people from working on the same exercise. If you have any questions while implementing the exercise, please also post them as comments in this issue.

Implementing the exercise means creating the following files:

<pre>
languages
└── csharp
    └── exercises
        └── concept
            └── arrays
                ├── .docs
                |   ├── after.md
                |   ├── cli.md
                |   ├── debug.md
                |   ├── hints.md
                |   ├── instructions.md
                |   └── introduction.md
                ├── .meta
                |   ├── config.json
                |   └── Example.cs
                ├── Arrays.csproj
                ├── Arrays.cs
                └── ArraysTest.cs
</pre>

### Step 1: adding track-specific files

These files are specific to the C# track:

- `Arrays.csproj`: the C# project file.
- `ArraysTest.cs`: the test suite.
- `Arrays.cs`. the stub implementation file, which is the starting point for students to work on the exercise.
- `.meta/Example.cs`: an example implementation that passes all the tests.

Check out the [`floating-point-numbers exercise`][csharp-docs-concept-exercises-floating-point-numbers] for an example on what these files should look like.

### Step 2: adding documentation files

- `.docs/introduction.md`: an introduction to the concept. It should be explicit about what the exercise teaches and maybe provide a brief introduction to the concepts, but not give away so much that the user doesn't have to do any work to solve the exercise. An example file can be found [here][csharp-docs-introduction.md].
- `.docs/instructions.md`: instructions for the exercise. It should explicitly explain what the user needs to do (define a method with the signature `X(...)` that takes an A and returns a Z), and provide at least one example usage of that function. If there are multiple tasks within the exercise, it should provide an example of each. An example file can be found [here][csharp-docs-instructions.md].
- `.docs/hints.md`: if the user gets stuck, we will allow them to click a button requesting a hint, which shows this file. We will softly discourage them using it. The file should contain both general and task-specific "hints". These hints should be enough to unblock almost any student. An example file can be found [here][csharp-docs-hints.md].
- `.docs/after.md`: once the user completes the exercise they will be shown this file, which gives them any bonus information or further reading about the concept taught. An example file can be found [here][csharp-docs-after.md].

### Step 3: adding other files

- `.meta/config.json`: metadata for the exercise. This includes defining the test file and the solution files, as well as a list of all the tests and the method they are calling. An example file can be found [here][csharp-meta-config.json].

### Step 4: updating files

- `languages/csharp/config.json`: a new entry should be added to the `"concept"` array, which is part of the `"exercises"` property:

```json
{
  "slug": "arrays",
  "uuid": "b6c532c9-1e89-4fbf-8f08-27f5befb5bb8",
  "concepts": ["basic-collections", "basic-arrays"],
  "prerequisites": ["basic-numbers"]
}
```

### Step 5: updating list of implemented exercises

- Add the exercise to the [list of implemented exercises][csharp-docs-concept-exercises].

### Inspiration

When implementing this exercise, it can be very useful to look at already implemented C# exercises like the [strings][csharp-docs-concept-exercises-strings], [dates][csharp-docs-concept-exercises-dates] or [floating-point numbers][csharp-docs-concept-exercises-floating-point-numbers] exercises. You can also check the [general array concept documentation][docs-v3-types-array] to see if any other languages have already implemented an arrays exercise.

## Representer

This exercise does not require any specific representation logic to be added to the [representer][csharp-representer].

## Analyzer

This exercise could benefit from having an [analyzer][csharp-analyzer] that can comment on:

- Difference between `for` vs `foreach` loops.

[docs.microsoft.com-arrays]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/arrays/
[docs.microsoft.com-collections]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/collections
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
[csharp-meta-config.json]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/numbers-floating-point/.meta/config.json
[csharp-docs-concept-exercises]: https://github.com/exercism/v3/tree/master/languages/csharp/exercises/concept/README.md
