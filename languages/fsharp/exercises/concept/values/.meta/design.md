# Design

## Goal

The goal of this exercise is to teach the student the basics of the Concept of Values in [F#][values].

## Learning objectives

- Know what a value is.
- Know how to define a binding.
- Know how to define a function.
- Know how to return a value from a function.
- Know how to call a function.
- Know that bindings are immutable.
- Know how type inference works for bindings.
- Know how to define scope using significant whitespace.
- Know how to define an integer.
- Know how to use mathematical operators on integers.

## Out of scope

- Generic values.
- Mutable values.
- Reference cells.
- Naming rules for identifiers.
- Double-quoted identifiers.
- Shadowing.
- Memory and performance characteristics.
- Returning functions.
- Recursive functions.
- Higher-order functions.
- Anonymous functions
- Inline functions.
- Pure functions.
- Partial application.
- Currying.
- Organizing functions in modules and namespaces.
- Visibility.

## Concepts

The Concepts this exercise unlocks are:

- `bindings-basic`: know what a value is; know how to define a binding; know that bindings are immutable; know how to define scope using significant whitespace.
- `functions-basic`: know how to define a function; know how to return a value from a function; know how to call a function.
- `type-inference-basic`: know what type inference is; know how type inference works for bindings.
- `integers-basic`: know how to define an integer; know how to use mathematical operators on integers.

## Prequisites

There are no prerequisites.

## Representer

This exercise does not require any specific representation logic to be added to the [representer][representer].

## Analyzer

This exercise could benefit from the following rules added to the the [analyzer][analyzer]:

- Check if the whitespace adheres to community defaults.
- Check if the `totalTimeInMinutes` function actually calls the `preparationTimeInMinutes` function.

[analyzer]: https://github.com/exercism/fsharp-analyzer
[representer]: https://github.com/exercism/fsharp-representer
[values]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/values/
