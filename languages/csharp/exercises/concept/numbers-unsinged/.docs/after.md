# After

Integral numeric types have signed and unsigned variants. Unsigned variants can represent double the range of their signed counter part. These days for most programs memory usage is not a problem so choosing bigger signed type is in most cases a better choice. When reaching the limits of the `long` type choosing to use the `ulong` type can still be a good choice.

The ranges integral numeric types can represent can be found in [Microsoft's documentation][docs.microsoft.com-integral].

## Arithmetic overflow

Arithmetic overflow can cause program crashes or unexpected results. [This video][computerphile] about how Gangnam Style broke YouTube is an good example of arithmetic overflow. In the exercise decimals where used in the calculations. This type forces to always throw an `OverflowException`, other types do not throw this exception by default. Using a `checked` context will prevent arithmetic overflow from happening and throw an `OverflowException` instead. Read more about `checked` and `unchecked` contexts in [Microsoft's documentation][docs.microsoft.com-signed].

[computerphile]: https://www.youtube.com/watch?v=vA0Rl6Ne5C8
[docs.microsoft.com-integral]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/integral-numeric-types
[docs.microsoft.com-signed]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/checked-and-unchecked
