The C# `enum` type represents a fixed set of named constants (an enumeration). Normally, one can only refer to exactly one of those named constants. However, sometimes it is useful to refer to more than one constant. To do so, one can mark the `enum` as one which constants are _flags_. By carefully assigning the values of each constant, one can use bitwise operators to add or remove references to one or more of the (flag) constants. A flags enum can be defined as follows:

```csharp
[Flags]
public enum PhoneFeatures
{
    Call = 1,
    Text = 2
}
```

To work with bits, C# supports the following operators:

- `~`: bitwise complement
- `<<`: left shift
- `>>`: right shift
- `&`: logical AND
- `|`: logical OR
- `^`: logial XOR

Here is an example how to use a bitwise operator:

```csharp
1 << 2
// => 4
```
