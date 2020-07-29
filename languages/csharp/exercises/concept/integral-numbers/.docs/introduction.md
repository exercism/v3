C#, like many statically typed languages, provides a number of types that represent integers, each with its own range of values. At the low end the `sbyte` type has a minimum value of -128 and a maximum value of 127. Like all the integer types these values are available as `<type>.MinValue` and `<type>.MaxValue`. At the high end the `long` type has a minimum value of -9,223,372,036,854,775,808 and a maximum value of 9,223,372,036,854,775,807. In between lie the `short` and `int` types.

Each of the above types is paired with an unsigned equivalent: `sbyte`/`byte`, `short`/`ushort`, `int`/`uint` and `long`/`ulong`. In all cases the range of the values is from 0 to the negative signed maximum times 2 plus 1.

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

A variable (or expression) of one type can easily be converted to another. For instance, in an assignment operation, if the type of the value being assigned (lhs) ensures that the value will lie within the range of the type being assigned to (rhs) then there is a simple assignment:

```csharp
uint ui = uint.MaxValue;
ulong ul = ui;    // no problem
```

On the other hand if the range of type being assigned from is not a subset of the assignee's range of values then a cast, `()` operation is required even if the particular value is within the assignee's range:

```csharp
short s = 42;
uint ui = (uint)s;
```

#### Casting

Casting is the method by which an expression of one integral type can be converted to another.

An expression can be cast to another type with the cast operator `(<type>)`.

```csharp
long l = 1000L;
int i = (int)l;
```

#### Casting

The following paragraphs discuss the casting of integral types.

##### Casting Primitive Types - Implicit

C#'s type system is somewhat stricter than _C_'s or Javascript's and as a consequence, casting operations are more restricted. Implicit casting takes place between two numeric types as long as the "to" type can preserve the scale and sign of the "from" type's value. Note in the documentation the exception for converting to real numbers where precision may be lost.

An implicit cast is not signified by any special syntax. For example:

```csharp
int myInt = 1729;
long myLong = myInt;
```

##### Casting Primitive Types - Explicit

Where numeric types cannot be cast implicitly you can generally use the explicit cast .

Where the value being cast cannot be represented by the "to" type because it is insufficiently wide or there is a sign conflict then an overflow exception may be thrown in the case of integers, or the "to" type variable may take a value of `Infinity` in the case of floats and doubles.

#### Casting Primitive Types - Examples

```csharp
int largeInt = Int32.MaxValue;
int largeNegInt = Int32.MinValue;
ushort shortUnsignedInt = ushort.MaxValue;

// implicit cast
int from_ushort = shortUnsignedInt;          // 65535
float from_int = largeInt;                   // -21474836E+09

// explicit cast
uint from_largeInt = (uint)largeInt;         // 2147483647
uint from_neg = (uint) largeNegInt;          // 2147483648 or OverflowException is thrown (if checked)

```

#### Bit conversion

The `BitConverter` class provides a convenient way of converting integer types to and from arrays of bytes.
