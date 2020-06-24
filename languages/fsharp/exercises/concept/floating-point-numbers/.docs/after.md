A floating-point number is a number with zero or more digits behind the decimal separator. Examples are `-2.4`, `0.1`, `3.14`, `16.984025` and `1024.0`.

F# has three floating-point types:

- `single`: 4 bytes (~6-9 digits precision). Written as `2.45f` or `2.45F`. The `float32` type can be used as an alias.
- `float`: 8 bytes (~15-17 digits precision). This is the most common type. Written as `2.45` or using exponent notation as `2.3E+32` or `2.3e+32`. The `double` type can be used as an alias.
- `decimal`: 16 bytes (28-29 digits precision). Normally used when working with monetary data, as its precision leads to less rounding errors. Written as `2.45m` or `2.45M`.

Each floating-point type has its own [precision, approximate range and size][floating-point-types]. The precision indicates how many digits after the digit separator can be stored. This means that trying to store PI in a `single` will only store the first 6 to 9 digits (with the last digit being rounded).

Converting between different floating-point types is done using the [conversion operators][conversion-operators]:

```fsharp
let floatFromSingle = float 2.45f
```

Always be careful when checking the values of floating-point types for equality, as values that can appear to represent the same value could actually be different. See [this article][precision-in-comparisons] for more information (the code examples are in C#).

You can find a short introduction to floating-point numbers at [0.30000000000000004.com][0.30000000000000004.com]. The [Float Toy page][evanw.github.io-float-toy] has a nice, graphical explanation how a floating-point numbers' bits are converted to an actual floating-point value.

[floating-point-types]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/literals
[precision-in-comparisons]: https://docs.microsoft.com/en-us/dotnet/api/system.double.equals#precision-in-comparisons
[0.30000000000000004.com]: https://0.30000000000000004.com/
[evanw.github.io-float-toy]: https://evanw.github.io/float-toy/
[conversion-operators]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/
