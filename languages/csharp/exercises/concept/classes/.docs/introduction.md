One of the core object-oriented concepts is _encapsulation_, which can refer to:

- A grouping of related data and behavior that can be treated as a single unit or object.
- Restricting access to some of the data and behavior.

In C#, encapsulation is enabled by the _class_ concept. A C# class can have data (fields) and behavior (methods) associated with them, which are referred to as its _members_. You can think of a class as a template for creating instances of that class.

Access to data and methods can be restricted through access modifiers, the two most common ones being:

- `public`: the member can be accessed by any code (no restrictions).
- `private`: the member can only be accessed by code in the same class.

The same two access modifiers can also be applied to classes:

- `public`: the class can.
- `private`: the member can only be accessed by code in the same class.

Besides data, classes also have behavior associated with them.

The `class` keyword is used to define a class. It is common to specify an _access modifier_, which influences what other classes can use the class. The most common access modifier is `public`, which means that other classes can use this class.

```csharp
public class Calculator
{
    // ...
}
```

[object-oriented-programming]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/object-oriented-programming
