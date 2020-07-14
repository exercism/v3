Namespaces are a way to avoid name clashes on anything but the most trivial code base.

The syntax is as follows:

```csharp
namespace MyNameSpace
{
    public class MyClass {}

    public class OtherClass {}
}
```

Types enclosed in namespaces can be accessed by prefixing the type name with the ubiquitous dot syntax. Alternatively, and more usually, you can place a `using` directive at the top of the file (or within a namespace) and type can be used without the prefix.

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

You can alias a namespace with the syntax `using My = MySpace;` and then use the alias anywhere that the namespace could be used.
