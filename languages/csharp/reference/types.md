# C&#35; types

##

- [bool](../../types/boolean.md)
- [string](../../types/string.md)
- [char](../../types/char.md)
- object
- Enum
- [Null](../../types/null.md)
- [Nullable type](../../types/nullable.md)

### Numeric

- [int](../../types/integer.md)
- [uint](../../types/integer.md)
- [byte](../../types/byte.md)
- [sbyte](../../types/byte.md)
- [short](../../types/short.md)
- [ushort](../../types/short.md)
- [long](../../types/long.md)
- [ulong](../../types/long.md)
- [double](../../types/double.md)
- [float](../../types/single.md)
- [decimal](../../types/decimal_number.md)
- [BigInteger](../../types/big_integer.md)

Note: as can be seen, there are many signed/unsigned data type combinations (e.g. `int`/`uint` and `long`/`ulong`). In practice, the unsigned data types are used far less often then the signed ones, so we should probably start with teaching the signed versions. The unsigned versions can then be taught later on.

### Collections

- [Array](../../types/array.md)
- [List](../../types/list.md)
- [Dictionary](../../types/map.md)
- [HashSet](../../types/set.md)
- [Stack](../../types/stack.md)
- [Queue](../../types/deque.md)
- Enumerable
- ConcurrentDictionary (and other concurrent )
- Non-generic collections

Note: The non-generic collections (such as `ArrayList`) are a remnant of the C# 1.0 days, and have been superseded by generic variants when generics were introduced in C# 2.0. As nobody uses the non-generic collections anymore, we'll ignore them.

### Composite

- [Class](../../types/class.md)
- [Struct](../../types/struct.md)
- [ValueTuple](../../types/tuple.md)
- [Tuple](../../types/tuple.md)

Note: these types are essentially a grouping of one or more other types. As such, it is important to have taught the student several of these other, basic types before teaching these composite types.

Note: the `Tuple` type has been superseded by the `ValueTuple` type, which is why we should only be teaching the latter.

### Other types

- `Span<T>`
- `Memory<T>`
- [Range](../../types/range.md)
- Event
- Delegate

## Resources

- Official language reference: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/
- https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/index
- https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/built-in-types-table
