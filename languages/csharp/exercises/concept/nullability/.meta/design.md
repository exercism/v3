# Design

## Goal

The goal of this exercise is to introduce the student to the concept of [Nullability in C#][null-keyword].

## Learning objectives

- Know of the existence of the `null` literal.
- Know what a `NullReferenceException` is and when it is thrown.
- Know how to compare a value to `null`.
- Know the difference between value and reference types regarding nullability, especially pre C# 8.0.
- Know how to define nullable reference and value types.
- Know about the null-related operators (`!`, `?`, `??`).
- Know about basic null checking by the compiler.

## Out of scope

- Nullable attributes.
- In-depth discussion of null checking by the compiler.
- Enabling C# 8 null checking.
- Casting using the `as` or `is` operator or using pattern matching.

## Concepts

This Concepts Exercise's Concepts are:

- `nullability`: know of the existence of the `null` literal; know what a `NullReferenceException` is and when it is thrown; know how to compare a value to `null`; know the difference between value and reference types regarding nullability; know how to define nullable reference and value types; know about the null-related operators; know about basic null checking by the compiler.

## Prerequisites

This Concept Exercise's prerequisites Concepts are:

- `strings`: strings will be compared to `null` and basic methods from strings will be called.
- `basics`: integers will be compared to `null`, arithmetic operations will be performed on integers, variables will be introduced and updated.
- `exceptions-basic`: explain how a `NullReferenceException` is thrown when accessing a `null` value.
- `for-loops`: strings will be processed and constructed iteratively.
- `memory-allocation`: reference and value types will be used in their nullable and non-nullable variants.

## Representer

This exercise does not require any specific representation logic to be added to the [representer][representer].

## Analyzer

This exercise does not require any specific logic to be added to the [analyzer][analyzer].

[analyzer]: https://github.com/exercism/csharp-analyzer
[representer]: https://github.com/exercism/csharp-representer
[null-keyword]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/null
