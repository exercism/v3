It is unlikely that you will come across much production code that does not make use of namespaces.

An example of the syntax is:

```csharp
namespace MyNameSpace
{
    public class MyClass {}

    public class OtherClass {}
}
```

Namespaces are typically introduced with a `using` directive unless there is an actual name clash.

```csharp
namespace MySpace
{
    public MyClass {}
}

new MySpace.MyClass();

namespace OtherSpace
{
    using MySpace;

    new MyClass();
}
```

This [article][using] clearly explains the ways in which the `using` directive can be used:

- `using`: avoid having to qualify types with namespace
- `using static`: avoid having to qualify members with types (a good example is `Math.Max()`).
- `using a = b`: substitute a more readable name for the namespace name.

#### Clash of namespaces

.NET addresses the issue of two namespaces with the same name. This issue is addressed with the [namespace alias qualifier][namespace-alias-qualifier] and the [extern alias][extern-alias].

One reason to mention this fairly niche subject is that you will often see the qualifier `global::` for namespaces, particularly in generated code. The intention here is to avoid confusion with a nested namespace or class name. By prefixing a namespace with `global::` you ensure that a top-level namespace is selected.

#### Note for Java developers

When comparing with the `import` of Java `packages` some differences and similarities should be noted:

- There is no equivalent with C# of importing specific types.
- `using static` and `import static` are equivalent.
- Unlike Java packages C# [assemblies][assemblies] have no impact on access levels but like the relationship between packages and jars they can span multiple assemblies.
- The relationship between file system and packages is not reflected in C#'s namespaces.

#### Reference

- [Namespaces][namespaces]: how to define and import namespaces.
- [Accessibility levels][accessibility-levels]: use the `public/internal/private` access modifiers.

[namespaces]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/namespaces/
[accessibility-levels]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/accessibility-levels
[namespace-alias-qualifier]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/namespace-alias-qualifier
[extern-alias]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/extern-alias
[using]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/using-directive
[assemblies]: https://docs.microsoft.com/en-us/dotnet/standard/assembly/
