The principal arithmetic and comparison operators can be adapted for use by your own classes and structs. This is known as _operator overloading_.

This [article][operator-overloading] is a thorough discussion of the syntax as well as which operators can be overloaded and those that can't.

Most operators have the form:

```csharp
static <return type> operaator <operator symbols>(<parameters>);
```

Cast operators have the form:

```csharp
static (explicit|implicit) operator <cast-to-type>(<cast-from-type> <parameter name>);
```

Syntax example for standard operators:

```csharp
struct Point
{
    decimal x;
    decimal y;

    public static bool operator ==(Point pt, Point ptOther)
    {
        return pt.x == ptOther.x && pt.y == ptOther.y;
    }

    public static bool operator !=(Point pt, Point ptOther)
    {
        return !(pt == ptOther);
    }

    public static implicit operator Point((decimal x, decimal y) xy)
    {
        var pt = new Point();
        pt.x = xy.x;
        pt.y = xy.y;
        return pt;
    }

    public static explicit operator (decimal x, decimal y)(Point pt)
    {
        return (pt.x, pt.y);
    }
}
```

It is often productive to implement an `Equals()` method and override it from the `==` operator. Similarly, for comparisons you can implement the `IComparable / CompareTo()` interface. In both cases you get to kill two birds with just over one stone.

## Note for Scala developers

Scala developers should note that you cannot create operators from symbols that are not currently used as operators. You can use only existing symbols for those operations where the documentation specifies that they can be overloaded.

#### Reference

This documentation of [operator overloading][operator-overloading] details the syntax.

[operator-overloading]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/operator-overloading
