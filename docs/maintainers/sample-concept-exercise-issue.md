# C#: new exercise - arrays

This issue describes a new `arrays` exercise that should be added to the [v3][docs-v3] [C# track][docs-v3-csharp].

## Goal

The goal of this exercise is to teach the student how the concept of [collections][docs-v3-types-collection] is implemented in [C#][docs.microsoft.com-collections]. We'll teach the student about collections by having the student work with one specific type of collection, namely the [array][docs-v3-types-array]. The students will learn to define arrays, iterate over array items, access items by index, and more.

Of the many available C# collection types, we chose to use the [array][docs-v3-csharp-types-array] collection type as the first collection type students will be taught for the following reasons:

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

This concepts exercise's concepts are:

- `basic-collections`
- `basic-arrays`

## Prequisites

As an array is a collection type, it holds zero or more instances of another type. That means it _has_ to depend on one or more other types. The most likely candidates are the `string` and `int` data types, as these are both interesting enough and easy to work with.

The `string` and `int` data types are introduced through the `basic-strings` and `basic-numbers` concepts, which means that one of these concepts would become a prerequisite.

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

### Step 2: adding documentation files

- `.docs/introduction.md`: an introduction to the concept. It should be explicit about what the exercise teaches and maybe provide a brief introduction to the concepts, but not give away so much that the user doesn't have to do any work to solve the exercise.
- `.docs/instructions.md`: instructions for the exercise. It should explicitly explain what the user needs to do (define a method with the signature `X(...)` that takes an A and returns a Z), and provide at least one example usage of that function. If there are multiple tasks within the exercise, it should provide an example of each.
- `.docs/hints.md`: if the user gets stuck, we will allow them to click a button requesting a hint, which shows this file. We will softly discourage them using it. The file should contain both general and task-specific "hints". These hints should be enough to unblock almost any
- `.docs/after.md`: once the user completes the exercise they will be shown this file, which gives them any bonus information or further reading about the concept taught.
- `.docs/debug.md`: explains how a user that is coding in the browser can still do "debugging."

These files are also all described in the [V3 readme][docs-v3].

### Step 3: adding other files

- `.meta/config.json`: metadata for the exercise. This includes defining the test file and the solution files, as well as a list of all the tests and the method they are calling.

### Step 4: updating files

- `languages/csharp/config.json`: a new entry should be added to the `"concept"` array, which is part of the `"exercises"` property.

### Step 5: updating issues

- `https://github.com/exercism/v3/issues/18`: this issue should be updated to include the new exercise and its (new) concepts.

### Inspiration

When implementing this exericse, it can be very useful to look at already implemented C# exercises like the [strings][docs-v3-csharp-concept-exercises-strings], [dates][docs-v3-csharp-concept-exercises-dates] or [floating-point numbers][docs-v3-csharp-concept-exercises-floating-point-numbers] exercises. You can also check the [general array concept documentation][docs-v3-types-array] to see if any other languages have already implemented an arrays exercise.

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
[docs-v3]: ./README.md
[docs-v3-types-array]: ./types/array.md
[docs-v3-types-collection]: ./types/collection.md
[docs-v3-csharp]: ./languages/csharp/README.md
[docs-v3-csharp-types-array]: ./languages/csharp/types/array.md
[docs-v3-csharp-concept-exercises-strings]: ./languages/csharp/concept-exercices/strings
[docs-v3-csharp-concept-exercises-dates]: ./languages/csharp/concept-exercices/dates
[docs-v3-csharp-concept-exercises-floating-point-numbers]: ./languages/csharp/concept-exercices/numbers-floating-point
[csharp-analyzer]: https://github.com/exercism/csharp-analyzer
[csharp-representer]: https://github.com/exercism/csharp-representer
