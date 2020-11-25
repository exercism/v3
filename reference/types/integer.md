# Integer

## The Concept

An integer is a whole number (no fractions). There are two basic types of integers

- Unsigned integers: only positive integers (0, 1, 2, ...).
- Signed integers: positive _and_ negative integers (..., -2, -1, 0, 1, 2).

### Sizes

Various programming languages _limit_ the range of values a single integer can hold. They may also have various variants that have different range restrictions. Theses names include, but are not limited to:

- [Bit][type-bit]
- [Byte][type-byte]
- [Char][type-char]
- [Short][type-short]
- [Long][type-long]
- [Word][type-word]

## What to cover

A student may have no understanding of floating point numbers as more than numbers with decimal points.

- Explain what integer types are supported in your langauge, and when to use them.
- Ensure that type conversions are understood if appropriate (e.g. rounding, overflows)
- Ensure that students know how to compare numbers to each other.

## Exercises

### Production Line Analysis

This exercise asks students to write code to analyse the production of an assembly line. The reference implementation (C#) teaches:

- Arithmetic
- Comparing numbers
- Converting between integers and floating point numbers.
- Conditionals

#### Implementations

| Track | Exercise                                               | Changes |
| ----- | ------------------------------------------------------ | ------- |
| C#    | [cars-assemble][implementation-csharp-production-line] | None    |
| F#    | [cars-assemble][implementation-fsharp-production-line] | None    |
| Go    | [numbers][implementation-go-production-line]           | None    |

[type-bit]: ./bit.md
[type-byte]: ./byte.md
[type-char]: ./char.md
[type-long]: ./long.md
[type-short]: ./short.md
[type-word]: ./word.md
[implementation-csharp-production-line]: ../../languages/csharp/exercises/concept/cars-assemble/.docs/introduction.md
[implementation-fsharp-production-line]: ../../languages/fsharp/exercises/concept/cars-assemble/.docs/introduction.md
[implementation-go-production-line]: ../../languages/go/exercises/concept/numbers/.docs/introduction.md
