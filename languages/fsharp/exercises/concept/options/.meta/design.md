# Design

## Goal

The goal of this exercise is to teach the student the basics of the Concept of Options in F#.

## Learning objectives

- Know of the existing of the `Option<T>` type and when to use it.
- Know how to define an option value.
- Know how to use pattern matching with options.
- Know of some basic functions in the `Option` module (like `Option.map`) to modify options.
- Know how options differ from `null`.
- Know what generic types are.

## Out of scope

- Custom computation expression to deal with options.
- Custom operators to deal with options applicatively (like `<*>`).
- Converting to and from other types (like arrays of lists).

## Concepts

The Concepts this exercise unlocks are:

- `options`: know of the existing of the `Option<T>` type and when to use it; know how to define an optional value; know of some basic functions in the `Option` module (like `Option.map`) to modify options; know how options differ from `null`; know how to use pattern matching with options.
- `generic-types`: know what generic types are.

## Prequisites

This exercise's prerequisites Concepts are:

- `discriminated-unions`: know that an option is implemented as a DU and know how to do pattern matching on a DU.
- `higher-order-functions`: how to use higher-order functions.

## Representer

This exercise does not require any specific representation logic to be added to the [representer][representer].

## Analyzer

This exercise does not require any specific logic to be added to the [analyzer][analyzer].

[analyzer]: https://github.com/exercism/fsharp-analyzer
[representer]: https://github.com/exercism/fsharp-representer
[lists]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/lists
