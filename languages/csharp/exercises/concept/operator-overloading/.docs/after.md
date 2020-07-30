The principal arithmetic and comparison operators can be adapted for use by your own classes and structs. This is known as _operator overloading_.

This [article][operator-overloading] is a thorough discussion of the syntax as well as which operators can be overloaded and those that can't.

Most operators have the form:

```csharp
static <return type> operaator <operator symbols>(<parameters>);
```

[Cast operators][ud-conversion-operators] have the form:

```csharp
static (explicit|implicit) operator <cast-to-type>(<cast-from-type> <parameter name>);
```

Syntax examples:

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

    public static Point operator *(Point pt, decimal scale)
    {
        var ptNew = new Point();
        ptNew.x = pt.x * scale;
        ptNew.y = pt.y * scale;
        return ptNew;
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

You should note that you cannot create operators from symbols that are not currently used as operators. You can use only existing symbols for those operations where the documentation specifies that they can be overloaded.

Note that the order of parameters is important where they differ in type. In the above example code `pt * 10m` is a legal expression whereas `10m * pt` will not compile.

#### Reference

This documentation of [operator overloading][operator-overloading] details the syntax.

[operator-overloading]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/operator-overloading
[ud-conversion-operators]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/user-defined-conversion-operators
