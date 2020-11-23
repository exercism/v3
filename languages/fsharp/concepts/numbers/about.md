One of the key aspects of working with numbers in F# is the distinction between integers (numbers with no digits after the decimal separator) and floating-point numbers (numbers with zero or more digits after the decimal separator).

The two most commonly used numeric types in F# are `int` (a 32-bit integer) and `float` (a 64-bit floating-point number).

```fsharp
let i = 123   // Type is `int`
let d = 54.29 // Type is `float`
```

Both integers and floating-point numbers can use the `_` character as a _digit separator_, which can help when defining large numbers:

```fsharp
let largeInt = 1_000_000
// => 1000000

let largeFloat = 9_876_543.21
// => 9876543.21
```

Arithmetic is done using the standard [arithmetic operators][arithmetic-operators] (`+`, `-`, `*`, etc.). Numbers can be compared using the [standard comparison operators][comparison-operators] (`<`, `>=`, etc.) and equality (`=`) and inequality (`<>`) operators.

```fsharp
5 * 6
// => 30

1.2 > 0.8
// => true

2 <> 4
// => true
```

To convert between numeric types, one has to use the built-in [conversion operator][conversion-operators]. These operators are named after the type they will be converting to.

```fsharp
let floatFromInt = float 2
let intFromFloat = int 3.39
```

When converting between types, always be careful. If a value cannot be represented by the target type, the value will _overflow_.

[conversion-operators]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/casting-and-conversions#arithmetic-types
[arithmetic-operators]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/symbol-and-operator-reference/arithmetic-operators
[comparison-operators]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/symbol-and-operator-reference/arithmetic-operators#summary-of-binary-comparison-operators
