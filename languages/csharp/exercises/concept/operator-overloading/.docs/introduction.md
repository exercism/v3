The principal arithmetic and comparison operators can be adapted for use by your own classes and structs. This is known as _operator overloading_.

Syntax Example:

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
}
```

Scala developers should note that you cannot create operators from symbols that are not currently used as operators. You can use only existing symbols for those operations where the documentation specifies that they can be overloaded.
