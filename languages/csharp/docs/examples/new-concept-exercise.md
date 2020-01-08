---
name: "[C#] Add new concept exercise: arrays"
about: Add the arrays concept exercise to the C# track
title: "[C#] Add new concept exercise: arrays"
labels: track/csharp, exercise/concept
assignees: ""
---

# C# - Add new concept exercise: arrays

This issue describes how to add a new [C# concept exercise][docs-concept-exercises] named `arrays`.

## Goal

The goal of the `arrays` exercise is to teach the student how the concept of [collections][general-docs-types-collection] is implemented in [C#][docs.microsoft.com-collections]. We'll teach the student about collections by having the student work with one specific type of collection, namely the [array]general-docs-types-array]. The students will learn to define arrays, iterate over array items, access items by index, and more.

Of the many available C# collection types, we chose to use the [array]docs-types-array collection type as the first collection type students will be taught for the following reasons:

- Arrays don't require the student to know about generics.
- Arrays are a common data type in many language.
- Arrays have a fixed length. No complexity in adding or removing elements.
- Arrays have a simple shorthand syntax. No need to understand how constructors work to define an array.

## Things to teach

After completing this exercise, the student should:

- Know of the existence of the `Array` collection type.
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

## Concepts

The concept this exercise teaches is:

- `basic-arrays`

## Prequisites

As an array is a collection type, it holds zero or more instances of another type. That means it _has_ to depend on one or more other types. The most likely candidates are the `string` and `int` data types, as these are both interesting enough and easy to work with.

The `string` and `int` data types are introduced through the `basic-strings` and `basic-numbers` concepts, which means that one of these concepts would become a prerequisite.

## Resources to refer to

Some suggestions for resources to use in the exercise's documentation file(s):

### Hints

- [Arrays][docs.microsoft.com-arrays]
- [Single-dimensional arrays][docs.microsoft.com-single-dimensional-arrays]
- [Usings foreach with arrays][docs.microsoft.com-foreach-with-arrays]

### After

- [Collections][docs.microsoft.com-collections]
- [Implicitly typed arrays][docs.microsoft.com-implicitly-typed-arrays]

As this is an introductory exercise, we should take care not to link to very advanced resources, to prevent overwhelming the student.

## Representer

This exercise does not require any specific representation logic to be added to the [representer][docs-representer].

## Analyzer

This exercise could benefit from having an [analyzer][docs-analyzer] that can comment on:

- Difference between `for` vs `foreach` loops.

## Implementing

Please check the [how to implement a concept exercise guide][docs-how-to-implement-a-concept-exercise] for details on how to implement this exercise.

## Help

If you have any questions while implementing the exercise, please post the questions as comments in this issue.

[exercises-concept-strings]: ./languages/csharp/concept-exercices/strings
[exercises-concept-dates]: ./languages/csharp/concept-exercices/dates
[docs-concept-exercises]: ./languages/csharp/docs/concept-exercises.md
[docs-analyzer]: ./languages/csharp/docs/analyzer.md
[docs-representer]: ./languages/csharp/docs/representer.md
[docs-how-to-implement-a-concept-exercise]: ./languages/csharp/docs/how-to-implement-a-concept-exercise.md
[docs.microsoft.com-arrays]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/arrays/
[docs.microsoft.com-collections]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/collections
[docs.microsoft.com-foreach-with-arrays]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/arrays/using-foreach-with-arrays
[docs.microsoft.com-single-dimensional-arrays]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/arrays/single-dimensional-arrays
[docs.microsoft.com-implicitly-typed-arrays]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/arrays/implicitly-typed-arrays
[general-docs-types-array]: ./types/array.md
[general-docs-types-collection]: ./types/collection.md
