# Floating-point number

A floating point number is a real number, which differ from whole numbers in that a real number also allows for exponent digits.

Programming languages may name their floating point type to the amount of _precision_ they can hold, whereas the following is based on [the IEEE 754 standard][wiki-ieee754]:

- [Half][type-half] (occupying 16 [bits][type-bit])
- [Single][type-single] (occupying 32 [bits][type-bit])
- [Double][type-double] (occupying 64 [bits][type-bit])
- Quadruple (occupying 128 [bits][type-bit])
- Octuple (occupying 256 [bits][type-bit])

## What to cover

A student may have no understanding of floating point numbers as more than numbers with decimal points.

- Explain that a floating point number is not _just_ a number with a decimal place.
- Explain what floating point types are used in your langauge, and when to use them.
- Ensure that type conversions are understood if appropriate (e.g. rounding, precision-changing)
- Ensure that students know how to compare numbers to each other.

## Exercises

### Production Line Analysis

This exercise asks students to write code to analyse the production of an assembly line. The reference implementation (C#) teaches:

- Arithmetic
- Comparing numbers
- Converting between integers and floating point numbers.
- Conditionals

#### Implementations

| Track | Exercise                                         | Changes |
| ----- | ------------------------------------------------ | ------- |
| C#    | [numbers][implementation-csharp-production-line] | None    |
| F#    | [numbers][implementation-fsharp-production-line] | None    |

### Savings Accounts

This exercise calculates interest on savings accounts. The reference implementation (C#) teaches:

- Rounding
- The importance of different precisions, using integers, 4 byte floating point numbers and 16 byte floating point numbers
- Basic loops

#### Implementations

| Track | Exercise                                                         | Changes                                                                                               |
| ----- | ---------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------- |
| C#    | [numbers-floating-point][implementation-csharp-savings-accounts] | None                                                                                                  |
| F#    | [numbers-floating-point][implementation-fsharp-savings-accounts] | Replaced third task that used while loop with task that uses an `int`/`double`/`decimal` combination. |

[type-bit]: ./bit.md
[type-double]: ./double.md
[type-half]: ./half.md
[type-single]: ./single.md
[wiki-ieee754]: https://en.wikipedia.org/wiki/IEEE_754
[implementation-csharp-production-line]: ../../languages/csharp/exercises/concept/numbers/.docs/introduction.md
[implementation-fsharp-production-line]: ../../languages/fsharp/exercises/concept/numbers/.docs/introduction.md
[implementation-csharp-savings-accounts]: ../../languages/csharp/exercises/concept/numbers-floating-point/.docs/introduction.md
[implementation-fsharp-savings-accounts]: ../../languages/fsharp/exercises/concept/numbers-floating-point/.docs/introduction.md
