It is unlikely that you will come across much production code that is not enclosed in namespaces. Namespaces are typically introduced with a `using` statement unless there is an actual name clash.

This [article][using] clearly explains the ways in which the `using` directive can be used:

- `using`: avoid having to qualify types with namespace
- `using static`: avoid having to qualify members with types (a good example is `Math.Max()`).
- `using a = b`: substitute a more readable name for the namespace name.

#### Clash of namespaces

.NET addresses the issue of two namespaces with the same name. This issue is addressed with the [namespace alias qualifier][namespace-alias-qualifier] and the [extern alias][extern-alias].

One reason to mention this fairly niche subject is that you will often see the qualifier `global::` for namespaces, particularly in generated code.  The intention here is to avoid confusion with a nested namespace or class name.

#### Note for Java developers

When comparing with the import of packages some differences and similarities should be noted:

- There is no equivalent with C# of importing specific types.
- `using static` and `import static` are equivalent.

#### Reference

- [Namespaces][namespaces]: how to define and import namespaces.
- [Accessibility levels][accessibility-levels]: use the `public/internal/private` access modifiers.

[namespaces]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/namespaces/
[accessibility-levels]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/accessibility-levels
[namespace-alias-qualifier]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/namespace-alias-qualifier
[extern-alias]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/extern-alias
