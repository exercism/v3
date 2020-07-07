C# `struct`s are closely related `class`s. They have state and behavior. They can have the same kinds of members, constructors, methods, fields, properties, etc fields and properties can be simple types, `struct`s and reference types. `struct`s observe the same rules about scope, read/write rules and access levels.

```csharp
enum Unit
{
    Kg,
    Lb
}
struct Weight
{
    private double count;
    private Unit unit;

    public Weight(double count, Unit unit)
    {
        this.count = count;
        this.unit = unit;
    }

    public override string ToString()
    {
        return count.ToString() + unit.ToString();
    }
}

new Weight(77.5, Unit.Kg).ToString();
// => "77.6Kg"
```

One of the main things to remember is that when one struct is assigned to a variable or passed as a parameter the values are copied across so changes to he original variable will not affect the copied one and vice versa. In summary, `struct`s are **value types**.

This [article][structs-patterns] discusses the differences between `struct`s and `class`s. You will see from the article that `struct`s tend to be lightweight and [immutable][structs-immutable] although this guidance is not enforced by the compiler or runtime.

There are a couple of things that you will come up against (and about which the compiler will remind you):

1. Members of a `struct` cannot be initialized inline.
2. A `struct` cannot be inherited

As a result of point 1 above there is no way for the developer of a `struct` to prevent invalid instances coming into existence.

#### Common structs

In most code the most commonly used library `struct` is the [`DateTime`][date-time] type.

Instances of `DateTime` behave much like numbers with comparison operators `>` and `<` and arithmetic operators but unlike equality testing using `Equals()` there is no free-ride with the operators. You have to [implement][operators] them yourself.

One thing to note about `DateTime` is that it implements a number of interfaces. Although `struct`s cannot be derived from other `struct`s they can implement interfaces.

#### Equality

Equality testing for `struct`s can often be much simpler than that for `class`s as it simply compares fields for equality by default. There is no need to override `object.Equals()` (or `GetHashCode()`). Remember that if you are relying on `Object.GetHashCode()` you must still ensure that the fields involved in generating the hash code (i.e. all the fields) must not change while a hashed collection is use. Effectively, this means that structs used in this way should be immutable. See (cross-ref-tba).

On the other hand, this [article][equality] describes how performance can be optimised by creating your own custom `Equals()` and `GetHashCode()` method as is often done with `class`s. The difference in the case of this exercise was about 20% in a not very rigorous comparison but that may be on the low side because all the fields are of the same type - see below.

There are discussions on the [web][equality-performance] about speed improvements, where the `Equals()` method is not overridden, if all fields are of the same type. The difference in this exercise of including disparate fields was about 60%. This is not mentioned in Microsoft's documentation so that makes it an un-documented implementation detail and it should be exploited judiciously.

```csharp
public bool Equals(Weight other)
{
    return count.Equals(other.count) && unit.Equals(other.unit);
}

public override bool Equals(object obj)
{
    return obj is Weight other && Equals(other);
}

public override int GetHashCode()
{
    return HashCode.Combine(count, unit);
}
```

#### General

- [structs][structs]: introduction to structs.
- [class-or-struct][class-or-struct]: lists the guidelines for choosing between a struct and class.

[structs-immutable]: https://stackoverflow.com/a/3753640/96167
[date-time]: https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netcore-3.1
[operators]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/operator-overloading
[equality]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/statements-expressions-operators/how-to-define-value-equality-for-a-type
[equality-performance]: https://medium.com/@semuserable/c-journey-into-struct-equality-comparison-deep-dive-9693f74562f1
[structs]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/struct
[class-or-struct]: https://docs.microsoft.com/en-us/dotnet/standard/design-guidelines/choosing-between-class-and-struct
