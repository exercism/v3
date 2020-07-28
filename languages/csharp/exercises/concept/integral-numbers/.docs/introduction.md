C#, like many statically typed languages, provides a number of types that represent integers each with its own range of values. At the low end the `sbyte` type has a minimum value of -128 and a maximum value of 127. Like all the integer types these values are available as `<type>.MinValue` and `<type>.MaxValue`. At the high end the `long` type has a minimum value of -9,223,372,036,854,775,808 and a maximum value of 9,223,372,036,854,775,807. In between lie the `short` and `int` types.

Each of the above types is paired with an unsigned equivalent: `sbyte`/`byte`, `short`/`ushort`, `int`/`uint` and `long`/`ulong`. In all cases the range of the values is from 0 to the signed maximum times 2 plus 1.

A variable (or expression) on one type can easily be converted to another. For instance, in an assignment operation, if the type of the value being assigned (lhs) ensures that the value will fit within the type being assigned (rhs) to then there is a simple assignment:

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

#### Bit conversion

The `BitConverter` class provides a convenient way of converting integer types to and from arrays of bytes.
