There are two different types of numbers in F#:

- Integers: numbers with no digits behind the decimal separator (whole numbers). Examples are `-6`, `0`, `1`, `25`, `976` and `500000`.
- Floating-point numbers: numbers with zero or more digits behind the decimal separator. Examples are `-2.4`, `0.1`, `3.14`, `16.984025` and `1024.0`.

The two most common numeric types in F# are `int` and `double`. An `int` is a 32-bit integer and a `double` is a 64-bit floating-point number.

## Converting between number types

Converting between number types is done through built-in conversion operators. These conversion operators are named after the type they will be converting to. F# does _not_ support automatic conversion between number types.
