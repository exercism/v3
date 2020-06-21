The coding exercise illustrates a number of properties of equality in C#:

### `Equals()`

- Simple types (strings and primitives) are typically tested for equality with the `==` and `!=`. This is considered more idiomatic than using the [`Equals()`][object-equals] method which is also available with these types. Java programmers should be alert to the fact that `==` compares by value in C# but by reference in Java when returning to their former language.
- Reference types (Instances of classes) are compared using the `Equals()` method inherited from `object`. If your goal with the equality test is to ensure that two objects are the exact same instance then relying on `object`'s implementation will suffice. If not, you need to overload `object.Equals()`.
- If you know that all the instances of your class are created in one place, say characters in some game simulation then reference equality is sufficient. However, it is likely that multiple instances of the same real-world entity will be created (from a database, by user input, via a web request). In this case values that uniquely identify the entity must be tested for equality. Therefore `Equals()` must be overridden.
- An overridden `Equals()` will contain equality tests on members of simple types using `==` and reference types with recursive calls to `Equals()`.

```csharp
class StatusBar
{
    private readonly int width = 200, height = 20;
    public override bool Equals(object other)
    {
        // ... null and type checks and performance optimisations
        return width == (other as StatusBar).width && height == (other as StatusBar).height;
    }
}
class Window
{
    private readonly string title = "Main";
    private readonly StatusBar statusBar = new StatusBar();
    public override bool Equals(object other)
    {
        // ... null and checks and performance optimisations
        return title == (other as Window).title && statusBar.Equals((other as Window).statusBar);
    }
}
```

- In addition to `public override bool Equals(object obj)` IDEs typically generate the overload `protected bool Equals(FacialFeatures other)` for use by derived classes.
- Do not use `==` unless you have overloaded the `==` operator, as well as the `Equals()` method in your class (see the `operator-overloading` exercise) or you care only that the references are equal.
- The static method `object.ReferenceEquals()` is used to compare two instances. This provides clarity and is a necessity where `Equals()` and `==` have been overloaded.

```csharp
var winA = new Window(); // above code that shows all windows are equal
var winB = new Window();
ReferenceEquals(winA, winB);
// => false
ReferenceEquals(winA, winA);
// => true
```

- Equality tests in `struct`s are dealt with in the `structs` exercise.
- Many developers rely on their IDEs to provide implementation of equality methods as these take care of all the minutiae of equality.
- Tests for the equality of [delegates][delegate-equality] is not specifically discussed in the exercise.
- There are no built in equality tests for arrays nor most collections. [LINQ][linq] (discussed in later exercises) provides [`SequenceEquals()`][sequence-equal] but in the absence of LINQ it is a matter of iterating through both collections and comparing items individually.
- For a discussion of how to use `==` and `!=` with your own classes see the `operator-overloading` exercise.

### `object.GetHashCode()`

- `object.GetHashCode()` returns a hash code in the form of a 32 bit integer. The hash code is used by dictionary and set classes such as `Dictionary<T>` and `HashSet<T>` to store and retrieve objects in a performant manner. In the case of dictionaries the hashing relates to the keys.
- There is an expectation amongst C# developers that if you override `Equals()` you will also override `GetHashCode()`. There is a relationship between `Equals()` and `GetHashCode()` that must hold true for correct behavour of dictionary and hash set classes and any others that use a hash code. You are expected to implement the method so that no traps are laid for maintainers who might add a hash code based collection at a later stage.
- The relationship between hash code and equality is that if two two objects are equal (`Equal()` returns true) then `GetHashCode()` for the two objects must return the same value. This does not apply in the reverse direction. It is not symmetrical. Picture a lookup function that first goes to a "bucket" using the hash code and then picks out the particular target using the equality test.
- The easiest way to create a hashcode is to call `HashCode.Combine()` passing in the values used in the equality test (or a subset). Bear in mind the more information you provide to `Combine()` the more performant the hash implementation is likely to be.
- It is possible that you can design a better hashcode than that produced by the library routines but either it's because you have an detailed understanding of the hashed collection's behavior or because it is a very simple collection where values can be used directly without hashing. It may not be worth the extra effort.
- The values used in the equality test must be stable while the hashed collection is in use. If you add an object to the collection with one set of values and then change those values the hash code will no longer point to the correct "bucket".

### Performance Enhancements

To improve performance slightly, especially where objects belong to collections you can add an overloaded member `public bool Equals(T other)`.

This will save a certain amount of null checking for reference types and will save a boxing step for value types as they will not need to be converted to an object (boxed) as an argument to `public override bool Equals(object other)`.

If you add the interface `IEquatable<T>` to your class this will require the overload to be implemented.

### `IEqualityComparer<T>`

If you have a class that can be uniquely identified in two different ways, say a `Person` class that has a SSID and a unique email address then .NET provides a means to allow two different collections to use different hash-code and equality tests. Each can take an different implementation of `IEqualityComparer<T>` which will provide an `Equals()` and a `GetHashCode()` method. You can have a dictionary keyed on SSID and another keyed on email address.

Where `IEqualityComparer<T>` is in play you would typically still implement `Equals()` and `GetHashCode()` on your item class to avoid problems outside the collection classes.

One consideration when using `IEqualityComparer<T>` is that privae methods etc. will not be available for the equality test.

### Note on floating point equality

One primitive that can challenge the unwary coder is testing the [equality of floating-point values][0.30000000000000004.com]. This is discussed in the _after.md_ document for the `floating-point-numbers` exercise.

### Equality and Inheritance

This [article][so-equals-inheritance] shows some of the decisions that have to be made with regard to equality and inheritance.

### General Informaton

- [Equality][equality]: how equality comparisons work in C#, including reference- and value type equality.
- [Equatable][equatable]: describes how to make a reference type use structural equality using `IEquatable<T>`.
- [Equality comparer][equality-comparer]: describes the `IEqualityComparer<T>` interface.
- [HashCode][hash-code]: how to create and combine hash codes
- [GetHashCode][get-hash-code]: API documentation
- [HashSet][hash-set]: API documentation

[equality]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/statements-expressions-operators/equality-comparisons
[equatable]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/statements-expressions-operators/how-to-define-value-equality-for-a-type
[equality-comparer]: https://docs.microsoft.com/en-us/dotnet/api/system.collections.generic.iequalitycomparer-1?view=netcore-3.1
[hash-set]: https://docs.microsoft.com/en-us/dotnet/api/system.collections.generic.hashset-1?view=netcore-3.1
[hash-code]: https://docs.microsoft.com/en-us/dotnet/api/system.hashcode?view=netcore-3.1
[get-hash-code]: https://docs.microsoft.com/en-us/dotnet/api/system.object.gethashcode?view=netcore-3.1
[delegate-equality]: https://docs.microsoft.com/en-us/dotnet/api/system.delegate.equals?view=netcore-3.1
[sequence-equal]: https://docs.microsoft.com/en-us/dotnet/api/system.linq.enumerable.sequenceequal?redirectedfrom=MSDN&view=netcore-3.1#System_Linq_Enumerable_SequenceEqual__1_System_Collections_Generic_IEnumerable___0__System_Collections_Generic_IEnumerable___0__
[linq]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/
[0.30000000000000004.com]: https://0.30000000000000004.com/
[so-equals-inheritance]: https://stackoverflow.com/questions/22154799/equals-method-inheritance-confusion
[object-equals]: https://docs.microsoft.com/en-us/dotnet/api/system.object.equals?view=netcore-3.1#System_Object_Equals_System_Object_
[so-hashcode-equals]: https://stackoverflow.com/questions/371328/why-is-it-important-to-override-gethashcode-when-equals-method-is-overridden
