## numbers

There are two different types of numbers in F#:

- Integers: numbers with no digits behind the decimal separator (whole numbers). Examples are `-6`, `0`, `1`, `25`, `976` and `500000`.
- Floating-point numbers: numbers with zero or more digits behind the decimal separator. Examples are `-2.4`, `0.1`, `3.14`, `16.984025` and `1024.0`.

The two most common numeric types in F# are `int` and `float`. An `int` is a 32-bit integer and a `float` is a 64-bit floating-point number.

Arithmetic is done using the standard arithmetic operators. Numbers can be compared using the standard numeric comparison operators and the equality (`=`) and inequality (`<>`) operators.

Converting between number types is done through built-in conversion operators. These conversion operators are named after the type they will be converting to. F# does _not_ support automatic conversion between number types.

## conditionals

In this exercise you must conditionally execute logic. The most common way to do this in F# is by using an `if/elif/else` statement:

```fsharp
if x = 5 then
    // Expression to evaluate when x equals 5
elif x > 7 then
    // Expression to evaluate when x greater than 7
else
    // Expression to evaluate in all other cases
```

The condition(s) used in an `if/elif/else` expression must be of type `bool`. F# has no concept of _truthy_ values.
