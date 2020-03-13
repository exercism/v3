One of the key aspects of working with numbers in F# is the distinction between integers (numbers with no digits after the decimal separator) and floating-point numbers (numbers with zero or more digits after the decimal separator).

The two most commonly used numeric types in F# are `int` (a 32-bit integer) and `double` (a 64-bit floating-point number).

Numbers can be compared using the default comparison operators (`<`, `>`, `==`, etc.). These operators can be used in [`if/elif/else` expressions][conditional-expression] to conditionally execute code. As `if/elif/else` is an expression, it can be returned from a function.

To convert between numeric types, one has to use the built-in [conversion operator][conversion-operators]. These operators are named after the type they will be converting to.

When converting between types, always be careful. If a value cannot be represented by the target type, the value will _overflow_.

[conditional-expression]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/conditional-expressions-if-then-else
[conversion-operators]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/casting-and-conversions#arithmetic-types
