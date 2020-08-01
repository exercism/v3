#### const

The `const` modifier can be (and generally should be) applied to any field where its value is known at compile time and will not change during the lifetime of the program.

```csharp
private const int num = 1729;
public const string title = "Grand" + " Master";
```

The compiler will guide you as to what expressions can be evaluated at compile-time. Simple arithmetic operations are allowed as is string concatenation. This excludes any evaluation that would require use of the heap or stack so all method calls and references to non-primitive types are not available.

There is some discussion on the web about the performance advantages of `const` over variables. In the case of a comparison with instance non-const fields there could be a noticeable saving in memory but compared to static variables that is decidedly trivial. Any consideration of CPU performance is likely to be seen by your colleagues as [premature optimization][premature-optimization].

A more compelling reason to use `const` is that it enhances a maintainer's ability to reason about the code. Glimpsing that a field is marked as `const` or `readonly` or that a property has no setter allows the maintainer largely to dismiss it from their analysis. It is unlikely to be the seat of bugs. It is unlikely to pose difficulties in a refactoring exercise. This [Stack Overflow comment][so-consts] addresses this.

The `const` modifier can also be applied to values within methods:

```csharp
public void Area(double r)
{
    const double π = 3.142;
    return Math.Pow((π * r), 2);
}
```

Identifying a value with `const` in this way can be useful if it is used multiple times in the method or you want to draw attention to its meaning. There is no performance gain over using literals inline.

#### readonly

The `readonly` modifier can be (and generally should be) applied to any field that cannot be made `const` where its value will not change during the lifetime of the program and is either set by an inline initializer or during instantiation (by the constructor or a method called by the constructor).

```csharp
private readonly int num;
private readonly Random rand = new Random();

public <constructor>(int num)
{
    this.num = num;
}
```

Use of the `readonly` modifier is encouraged for the same reasons that apply to `const`. The practice of constraining fields in this way helps maintainers reason about the code.

Note that adding the `readonly` modifier to a field prevents only the value of the field from being changed. In the case of reference types it does not protect the fields or properties of that type. In particular, it does not protect the contents of arrays.

```csharp
private readonly IList list = new List();

list = new List();  // does not compile

list.Add("new stuff");  // succeeds at runtime
```

To ensure that all members of a reference type are protected the fields can be made `readonly` and automatic properties can be defined without a `set` accessor.

You should examine [read-only collections][readonly-collections] in the Base Class Library.

For arrays the closest you can get to a read-only version is the [`Array.AsReadOnly<T>()][as-read-only] method.

#### Defensive Copying

Reflecting back on the coding exercise, imagine you have a code-base of several hundred thousand lines. You are passed the dictionary of developers into some method you are developing. Perhaps you have been tasked with printing out details of privileged developers. You decide to blank out the eye color in the dictionary to protect the developers' privacy. Unless a [deep copy] of the dictionary was made in the `Authenticator.GetDevelopers()` method, or, even better, it was wrapped in a read-only collection then you will have just trashed the authenticator.

This follows the principle of [defensive copying][defensive-copying]. You can make sure your formal API is not circumvented by avoiding exposure to callers of internal writeable state.

#### Reference

- [Readonly fields][readonly-fields]: how to define a readonly field.
- [Constants][constants]: how to define constants.

[readonly-fields]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/readonly#readonly-field-example
[constants]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/constants
[so-consts]: https://stackoverflow.com/a/5834473/96167
[premature-optimization]: https://wiki.c2.com/?PrematureOptimization
[so-deep-copy]: https://stackoverflow.com/questions/78536/deep-cloning-objects
[defensive-copying]: https://www.informit.com/articles/article.aspx?p=31551&seqNum=2
[as-read-only]: https://docs.microsoft.com/en-us/dotnet/api/system.array.asreadonly?view=netcore-3.1
[readonly-collections]: https://docs.microsoft.com/en-us/dotnet/api/system.collections.objectmodel.readonlycollection-1?view=netcore-3.1
