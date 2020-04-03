# Design

## Goal

The goal of this exercise is to teach the student the basics of the Concept of Discriminated Unions in [F#][discriminated-unions].

## Learning objectives

- Know what discriminated unions are.
- Know how discriminated unions are different from enums.
- Know how to define a discriminated union, with and without data.
- Know how to use `match` to pattern match on discriminated unions.
- Know what pattern matching is.
- Know about constant, identifier and wildcard patterns.
- Know how to apply guards to patterns.
- Know about exhaustiveness checking in pattern matching.

## Out of scope

- Recursive discriminated unions.
- Single type wrapper discriminated unions.
- Active patterns.
- Adding members to discriminated unions.
- `function` pattern shorthand notation.

## Concepts

The Concepts this exercise unlocks are:

- `discriminated-unions`: know what discriminated unions are; know how discriminated unions are different from enums; know how to define a discriminated union, with and without data; know how to use `match` to pattern match on discriminated unions.
- `pattern-matching`: know what pattern matching is; know about constant, identifier and wildcard patterns; know how to apply guards to patterns; know about exhaustiveness checking in pattern matching.

## Prequisites

This exercise's prerequisites Concepts are:

- `basics`: defining functions and scoping and using integers.

## Representer

This exercise does not require any specific representation logic to be added to the [representer][representer].

## Analyzer

This exercise does not require any specific logic to be added to the [analyzer][analyzer].

[analyzer]: https://github.com/exercism/fsharp-analyzer
[representer]: https://github.com/exercism/fsharp-representer
[discriminated-unions]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/discriminated-unions
