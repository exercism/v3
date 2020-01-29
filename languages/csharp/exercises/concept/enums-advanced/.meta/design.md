# Design

## Goal

The goal of this exercise is to teach the student advanced aspects of the Concept of Enums in [C#][docs.microsoft.com-bitwise-and-shift-operators]. We'll do this in the form of working with [flag][docs.microsoft.com-flagsattribute] enums, which are enums whose values are interpreted as bitwise flags that can be manipulated through bitwise operations.

## Things to teach

After completing this exercise, the student should:

- Know what a flags enumeration is.
- Know how to define a flags enumeration.
- Know how to check if a flag has been set on an enum value.
- Know how to set a flag on an enum value.
- Know how to unset a flag on an enum value.
- Know that an enum's underlying type can be changed.

## Things not to teach

As this is an advanced exercise, there are no enum-related things that we should explicitly _not_ teach.

## Resources to refer to

Here are some suggestions for resources to use in the exercise's documentation file(s):

### Hints

- [`switch` statement][docs.microsoft.com-switch-keyword]
- [Bitwise and shift operators][docs.microsoft.com-bitwise-and-shift-operators]

### After

- [Working with enums as bit flags][docs.microsoft.com-enumeration-types-as-bit-flags].
- [Enum flags and bitwise operators][alanzucconi.com-enum-flags-and-bitwise-operators]

## Concepts

The Concepts this exercise unlocks are:

- `enums-advanced`: know how to define a "flags" enum; know how to add, remove or check for flags; know how to change the underlying type of an enum.

## Prequisites

This exercise's prerequisites Concepts are:

- `enums-basic`: know how to define the `enum`.
- `attributes-basic`: know how to annotate the enum with the `[Flags]` attribute.
- `bitwise-operations`: know how to use bitwise operations to work with the flag enum values.

## Representer

This exercise does not require any specific representation logic to be added to the [representer][representer].

## Analyzer

This exercise could benefit from having an [analyzer][analyzer] that can comment on:

- Verify that the `Permission` enum is marked with the `[Flags]` attribute.
- Suggest using `byte` as the enum's backing type if no backing type was explicitly specified.

[analyzer]: https://github.com/exercism/csharp-analyzer
[representer]: https://github.com/exercism/csharp-representer
[docs.microsoft.com-enumeration-types-as-bit-flags]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/enumeration-types#enumeration-types-as-bit-flags
[docs.microsoft.com-bitwise-and-shift-operators]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/bitwise-and-shift-operators
[docs.microsoft.com-switch-keyword]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/switch
[docs.microsoft.com-binary-notation]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/integral-numeric-types#integer-literals
[docs.microsoft.com-flagsattribute]: https://docs.microsoft.com/en-us/dotnet/api/system.flagsattribute?view=netcore-3.1
[alanzucconi.com-enum-flags-and-bitwise-operators]: https://www.alanzucconi.com/2015/07/26/enum-flags-and-bitwise-operators/
[concept-bitwise-manipulation]: ../../../../../reference/concepts/bitwise_manipulation.md
