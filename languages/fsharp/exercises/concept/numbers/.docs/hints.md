### 1. Calculate the production rate per second

- Determining the success rate can be done through a [conditional expression][conditional-expression].
- F# does not allow for multiplication to be applied to two different number types (such as an `int` and a `double`). One thus has to convert one of the number types to the other number type using one of the built-in [conversion operators][conversion-operators].

### 2. Calculate the number of working items produced per second

- Converting from a `double` to an `int` can be done through one of the built-in [conversion operators][conversion-operators].

[conditional-expression]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/conditional-expressions-if-then-else
[conversion-operators]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/casting-and-conversions#arithmetic-types
