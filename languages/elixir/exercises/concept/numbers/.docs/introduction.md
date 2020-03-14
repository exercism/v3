There are two different types of numbers in Elixir:

- Integers: numbers with no digits behind the decimal separator (whole numbers). Examples are `-6`, `0`, `1`, `25`, `976` and `500000`.
- Floating-point numbers: numbers with zero or more digits behind the decimal separator. Examples are `-2.4`, `0.1`, `3.14`, `16.984025` and `1024.0`.

The two most common numeric types in Elixir are `integer` and `float`. An `integer` is a 32-bit integer and a `float` is a 64-bit double precision floating-point number.

Converting between number types is done through built-in conversion operators. These conversion operators are named after the type they will be converting to. Elixir automatically converts between number types -- `integer` types convert to `float` for arithmetic operations.

In this exercise you must conditionally execute logic. The most common way to do this in Elixir is by using a `cond` control flow structure.
