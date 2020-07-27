As with many strongly typed languages integers come in a number of shapes and sizes: byte (8 bits), unsigned short (ushort - 16 bits), unsigned int (uint - 32 bits) and unsigned long (ulong - 64 bits) together with their signed equivalent: sbyte, short, int and long.

To convert an expression of a type with more bits to one with fewer you must use the cast operator, `()`:

```csharp
int wide;
short narrow = (short)wide;
```
