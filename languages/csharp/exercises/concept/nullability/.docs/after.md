Sometimes we need to make it so that variables have no particular
value, i.e. they are empty.  In C#, this corresponds to the [literal
`null`][null-keyword]. 

In this exercise, we saw the definition of [nullable
types][nullable-types-tutorial], and how the compiler and runtime of
C# help us dealing with `null` values.

At compilation time, [the operator `?`][nullable-reference-types] will
declare a variable as being *nullable*. The compiler will then try to
help us avoiding calling methods on possibly `null` variables, by
raising warnings. You can use [the operator
`!`][null-forgiving-operator] to avoid warnings in places we are sure
a nullable variable is not null, but the compiler cannot detect
it. Finally, we can use [the operator `??`][null-coalescing-operator]
to provide a default value for a nullable variable.

At run time, calling any method or property on a
`null` value throws a `NullReferenceException` exception.
That is why it is important to always check if a nullable variable
is `null` before calling methods on its value.

# Important changes in C# 8.0

Sometimes, we need to make sure that some variables are never
`null`. This simplifies code because it won't need to handle
`NullReferenceException`s or provide extra provisions for `null`
values.

Before C# 8.0, reference types were nullable by default. For example,
a variable of type `string` may contain a `null` value, even it is not
declared as `string?`.

For more information, refer to [this
][nullable-csharp-8] and [this][nullable-reference-types-tutorial] documents.

[null-keyword]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/null
[nullable-types-tutorial]: https://csharp.net-tutorials.com/data-types/nullable-types/
[nullable-reference-types]: https://docs.microsoft.com/en-us/dotnet/csharp/nullable-references
[nullable-csharp-8]: https://docs.microsoft.com/en-us/dotnet/csharp/nullable-references
[null-forgiving-operator]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/null-forgiving
[null-coalescing-operator]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/null-coalescing-operator
[nullable-reference-types-tutorial]: https://docs.microsoft.com/en-us/archive/msdn-magazine/2018/february/essential-net-csharp-8-0-and-nullable-reference-types
