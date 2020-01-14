# Floating-point number

A floating point number is a real number, which differ from whole numbers in that a real number also allows for exponent digits.

Programming languages may name their floating point type to the amount of _precision_ they can hold, whereas the following is based on [the IEEE 754 standard][wiki-ieee754]:

- [Half][type-half] (occupying 16 [bits][type-bit])
- [Single][type-single] (occupying 32 [bits][type-bit])
- [Double][type-double] (occupying 64 [bits][type-bit])
- Quadruple (occupying 128 [bits][type-bit])
- Octuple (occupying 256 [bits][type-bit])

# Implementations

- [C# (basic)][implementation-csharp-basic]
- [C# (advanced)][implementation-csharp-advanced]

[type-bit]: ./bit.md
[type-double]: ./double.md
[type-half]: ./half.md
[type-single]: ./single.md
[wiki-ieee754]: https://en.wikipedia.org/wiki/IEEE_754
[implementation-csharp-basic]: ../../languages/csharp/exercises/concept/numbers/.docs/introduction.md
[implementation-csharp-advanced]: ../../languages/csharp/exercises/concept/numbers-floating-point/.docs/introduction.md
