The principal arithmetic and comparison operators can be adapted for use by your own classes and structs. This is known as _operator overloading_.

Most operators have the form:

```csharp
static <return type> operaator <operator symbols>(<parameters>);
```

Cast operators have the form:

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

You cannot create operators from symbols that are not currently used as operators. You can use only existing symbols for those operations where the documentation specifies that they can be overloaded.
