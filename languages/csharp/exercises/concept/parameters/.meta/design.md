## Learning objectives

- Know the difference between value and reference type parameters.
- Know how to pass value types by reference using the `ref`, `out` and `in` modifiers.

## Out of scope

- Overload resolution.
- ref returns.
- ref locals.

## Concepts

This Concepts Exercise's Concepts are:

- `parameters`: know the difference between value and reference type parameters; know how to pass value types by reference using the `ref`, `out` and `in` modifiers.

## Prequisites

This Concept Exercise's prerequisites Concepts are:

- `memory-allocation`: know what value and reference types are and how they relate to memory usage, the heap and the stack.

Any data types used in this exercise (e.g. `strings`) should also be added as prerequisites.

## Resources to refer to

### Hints

- [passing-parameters][passing-parameters]: explains how values can be passed as arguments.
- [ref-parameter][ref-parameter]: describes how `ref` parameters work.
- [out-parameter][out-parameter]: describes how `out` parameters work.
- [in-parameter][in-parameter]: describes how `in` parameters work.

### After

- [passing-parameters][passing-parameters]: explains how values can be passed as arguments.
- [ref-parameter][ref-parameter]: describes how `ref` parameters work.
- [out-parameter][out-parameter]: describes how `out` parameters work.
- [in-parameter][in-parameter]: describes how `in` parameters work.

## Representer

This exercise does not require any specific representation logic to be added to the [representer][representer].

## Analyzer

This exercise does not require any specific analyzer logic to be added to the [analyzer][analyzer].

## Implementing

To implement this exercise, please [follow these instructions](https://github.com/exercism/v3/blob/master/languages/csharp/reference/implementing-a-concept-exercise.md).

## Help

If you have any questions while implementing the exercise, please post the questions as comments in this issue.

[how-to-implement-a-concept-exercise]: https://github.com/exercism/v3/blob/master/docs/maintainers/generic-how-to-implement-a-concept-exercise.md
[implemented-exercises]: https://github.com/exercism/v3/tree/master/languages/csharp/exercises/concept/README.md#implemented-exercises
[reference]: https://github.com/exercism/v3/blob/master/languages/csharp/reference/README.md#reference-docs
[reference-example]: https://github.com/exercism/v3/blob/master/reference/types/string.md#implementations
[analyzer]: https://github.com/exercism/csharp-analyzer
[representer]: https://github.com/exercism/csharp-representer
[exercise-example]: https://github.com/exercism/v3/tree/master/languages/csharp/exercises/concept/numbers-floating-point
[design-example]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/numbers/.meta/design.md
[config.json-example]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/numbers/.meta/config.json
[concept-exercises]: https://github.com/exercism/v3/blob/master/docs/concept-exercises.md
[passing-parameters]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-references/passing-parameters
[ref-parameter]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/ref#passing-an-argument-by-reference
[in-parameter]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/in-parameter-modifier
[out-parameter]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/out-parameter-modifier
