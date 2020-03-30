Sometimes we need to make it so that variables have no particular
value, i.e. they are empty.  In C#, this corresponds to the literal
`null`. 

In this exercice, we saw the definition of nullable types, and how the
compiler and runtime of C# help us dealing with `null` values.

At compilation time, the operator `?` will declare a variable as being
*nulable*. The compiler will then try to help us avoiding callig
methods on possibly `null` variables, by raising warnings. You can use
the operator `!` to avoid warnings in places we are sure a nullable
variable is not null, but the compiler cannot detect it. Finally, we
can use the operator `??` to provide a default value for a nullable
variable.

At execution time, we saw that if we try to call any method on a
`null` value, the runtime raises a `NullReferenceException`
exception. That is why it is important to always check if a variable
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
documentation][nullable-csharp-8].

# Additional Resources

- [Null keyword][null-keyword]: reference documentation for `null`
  keyword.
- [Nullable types tutorial][nullable-types-tutorial]: basic tutorial on how to work with nullable types.
- [Nullable reference types][nullable-reference-types]: how to use nullable reference types.
- [Null-coalescing operator][null-coalescing-operator]: explains how the null-coalescing operator works.
- [Null-forgiving operator][null-forgiving-operator]: explains how the null-forgiving operator works.
- [Null-conditional operator][null-conditional-operator]: explains how the null-conditional operator works.
- [Nullable reference types tutorial][nullable-reference-types-tutorial]: tutorial on nullable reference types.
- [string.IsNullOrEmpty][isnull-or-empty] and [string.IsNullOrWhiteSpace][isnull-or-whitespace]: useful checks for empty, whitespace or null strings.

[null-keyword]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/null
[nullable-types-tutorial]: https://csharp.net-tutorials.com/data-types/nullable-types/
[null-coalescing-operator]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/null-coalescing-operator
[null-forgiving-operator]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/null-forgiving
[null-conditional-operator]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/conditional-operator
[nullable-reference-types]: https://docs.microsoft.com/en-us/dotnet/csharp/nullable-references
[nullable-reference-types-tutorial]: https://docs.microsoft.com/en-us/archive/msdn-magazine/2018/february/essential-net-csharp-8-0-and-nullable-reference-types
[isnull-or-empty]: https://docs.microsoft.com/en-Us/dotnet/api/system.string.isnullorempty
[isnull-or-whitespace]: https://docs.microsoft.com/en-Us/dotnet/api/system.string.isnullorwhitespace
[nullable-csharp-8]: https://docs.microsoft.com/en-us/dotnet/csharp/nullable-references
