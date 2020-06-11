# Design

## Goal

The goal of this exercise is to teach the student the basics of programming in [Swift][thebasics].

## Learning objectives

- Know what a value is.
- Know how to define a constant.
- Know how to define a variable.
- Know how to define a function.
- Know how to return a value from a function.
- Know how to call a function.
- Know that constants are immutable.
- Know that variables are mutable.
- Know how type inference works.
- Know how to define an integer.
- Know how to use mathematical operators on integers.
- Know how to define single- and multi-line comments.

## Out of scope

- Generic values.
- Naming rules for identifiers.
- Back-quoted identifiers.
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
- Organizing functions in modules and frameworks.
- Visibility.

## Concepts

The Concepts this exercise unlocks are:

- `basics`: know what a value is; know how to define a constant; know how to define a variable; know how to define a function; know how to return a value from a function; know how to call a function; know that constants are immutable; know that variables are mutable; know how type inference works; know how to define an integer; know how to use mathematical operators on integers; know how to define single- and multi-line comments.

## Prequisites

There are no prerequisites.

## Representer

This exercise does not require any specific representation logic to be added to the representer (not yet implemented). <!--[representer][representer].-->

## Analyzer

This exercise could benefit from the following rules added to the the analyzer (not yet implemented). <!--[analyzer][analyzer]:-->

- Verify that the naming adheres to community defaults.
- Verify that constants are preferred to variables.
- Verify that the `remainingMinutesInOven` function uses the `expectedMinutesInOven` binding.
- Verify that the `totalTimeInMinutes` function actually calls the `preparationTimeInMinutes` function.

<!--[analyzer]: https://github.com/exercism/fsharp-analyzer
[representer]: https://github.com/exercism/fsharp-representer
-->

[thebasics]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html
