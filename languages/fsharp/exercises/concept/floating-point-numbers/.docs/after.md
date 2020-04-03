There are three floating-point types in F#: `double`, `single` and `decimal`. The most commonly used type is `double`, whereas `decimal` is normally used when working with monetary data. A `double` is written as `2.45`, a single as `2.45f` and a decimal as `2.45m`.

Each floating-point type has its own [precision, approximate range and size][floating-point-types].

Always be careful when checking the values of floating-point types for equality, as values that can appear to represent the same value could actually be different. See [this article][precision-in-comparisons] for more information (the code examples are in C#).

You can find a short introduction to floating-point numbers at [0.30000000000000004.com][0.30000000000000004.com]. The [Float Toy page][evanw.github.io-float-toy] has a nice, graphical explanation how a floating-point numbers' bits are converted to an actual floating-point value.

[floating-point-types]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/literals
[precision-in-comparisons]: https://docs.microsoft.com/en-us/dotnet/api/system.double.equals#precision-in-comparisons
[0.30000000000000004.com]: https://0.30000000000000004.com/
[evanw.github.io-float-toy]: https://evanw.github.io/float-toy/
