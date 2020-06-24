## 1. Calculate the production rate per second

- Determining the success rate can be done through a [conditional expression][conditional-expression].
- F# does not allow for multiplication to be applied to two different number types (such as an `int` and a `float`). One thus has to convert one of the number types to the other number type using one of the built-in [conversion operators][conversion-operators].
- Numbers can be compared using the built-in [comparison operators][comparison-operators].

## 2. Calculate the number of working items produced per second

- Converting from a `float` to an `int` can be done through one of the built-in [conversion operators][conversion-operators].

[conditional-expression]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/conditional-expressions-if-then-else
[conversion-operators]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/casting-and-conversions#arithmetic-types
[comparison-operators]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/symbol-and-operator-reference/arithmetic-operators#summary-of-binary-comparison-operators
