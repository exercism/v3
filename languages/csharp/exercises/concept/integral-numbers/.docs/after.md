C#, like many statically typed languages, provides a number of types that represent integers, each with its own range of values. At the low end the `sbyte` type has a minimum value of -128 and a maximum value of 127. Like all the integer types these values are available as `<type>.MinValue` and `<type>.MaxValue`. At the high end the `long` type has a minimum value of -9,223,372,036,854,775,808 and a maximum value of 9,223,372,036,854,775,807. In between lie the `short` and `int` types.

Each of the above types is paired with an unsigned equivalent: `sbyte`/`byte`, `short`/`ushort`, `int`/`uint` and `long`/`ulong`. In all cases the range of the values is from 0 to the negative signed maximum times 2 plus 1.

A variable (or expression) of one type can easily be converted to another. For instance, in an assignment operation, if the type of the value being assigned (rhs) ensures that the value will fit within the range of the type being assigned to (lhs) then there is a simple assignment:

```csharp
ulong ul;
uint ui = uint.MaxValue;
ul = ui;    // no problem
```

On the other hand if the range of type being assigned from is not a subset of the assignee's range of values then a cast, `()` operation is required even if the particular value is within the assignee's range:

```csharp
uint ui;
short s = 42;
ui = (uint)s;
```

In the above example, if the value lay instead outside the range of the assignee then an overflow would occur. See (TODO cross-ref-tba).

The multiplicity of integer types reflects machine architectures, in the size of registers, the size of CPU instruction arguments and the treatment of sign within the CPU. A value of type `long` uses 64 bits whereas a value of type `sbyte` uses 8 bits. In some cases there will be implications on CPU performance, memory usage and even disk usage (where smaller integer sizes will improve operations). Selection of integer `type` can also be a rough and ready wsy of communicating information to other developers about the expected range of values. The `int` type is widely used as the default type where nothing special has been identified about the particular usage. The `long` or `ulong` is widely used as a simple identifier.

The types discussed so far are _primitive_ types. Each is paired with a `struct` alias which implements fields (such as `MinValue`) which are associated with the type. Some examples are: `sbyte` / `SByte`, `ushort` / `UInt16` and `long` / `Int64`.

|        | Width  | Minimum                    | Maximum                     |
| ------ | ------ | -------------------------- | --------------------------- |
| sbyte  | 8 bit  | -128                       | +127                        |
| short  | 16 bit | -32_768                    | +32_767                     |
| int    | 32 bit | -2_147_483_648             | +2_147_483_647              |
| long   | 64 bit | -9_223_372_036_854_775_808 | +9_223_372_036_854_775_807  |
| byte   | 8 bit  | 0                          | +255                        |
| ushort | 16 bit | 0                          | +65_535                     |
| uint   | 32 bit | 0                          | +4_294_967_295              |
| ulong  | 64 bit | 0                          | +18_446_744_073_709_551_615 |

#### Bit conversion

The `BitConverter` class provides a convenient way of converting integer types to and from arrays of bytes.

#### Reference

- [Integral numeric types][integral-numeric-types]: overview of the integral numeric types.
- [Numeric conversions][numeric-conversions]: overview of implicit and explicit numeric conversions.

[integral-numeric-types]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/integral-numeric-types
[numeric-conversions]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/numeric-conversions
