# Design

## Goal

The goal of this exercise is to teach the student how the Concept of `signed` and `unsigned` numbers is implemented in [C#][docs.microsoft.com-integral]. It will introduce this concept introducing the the student deal to calculations that can produce very large numbers. This exercise will also let the the student think about arithmetic overflow and the `OverflowException`.

## Learning objectives

- Know of the existence of `signed` and `unsigned` integral numeric types.
- Understand that an integral numeric type can represent a range of numbers.
- Understand that an arithmetic overflow can happen.
- Know how to deal with an `OverflowException`.

## Out of scope

- Changing between `checked` and `unchecked` contexts.

## Concepts

The Concepts this exercise unlocks are:

- `numbers-signedness`: know of the existence of `signed` and `unsigned` integral numeric types; understand that `signed` types can represent both negative and positive numbers and `unsigned` only positive numbers and double the range of it's signed counter part; understand arithmetic overflow.

## Prerequisites

This exercise's prerequisites Concepts are:

- `numbers-basic`: define numbers and apply arithmetic and logic to them.
- `type-conversion-numbers`: convert from a floating-point type to an integral number.
- `exceptions`: catch a specific exception and handle it.

## Representer

This exercise does not require any specific representation logic to be added to the [representer][representer].

## Analyzer

This exercise does not require any specific logic to be added to the [analyzer][analyzer].

[analyzer]: https://github.com/exercism/csharp-analyzer
[representer]: https://github.com/exercism/csharp-representer
[docs.microsoft.com-integral]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/integral-numeric-types
