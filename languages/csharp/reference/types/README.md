# C&#35; types

This document lists the most important C# types.

## Basic

- [`bool`][bool]
- [`string`][string]
- [`char`][char]
- `object`
- `enum`
- [`null`][null]

## Numeric

- [`int`][int]
- [`uint`][int]
- [`byte`][byte]
- [`sbyte`][byte]
- [`short`][short]
- [`ushort`][short]
- [`long`][long]
- [`ulong`][long]
- [`double`][double]
- [`float`][float]
- [`decimal`][decimal]
- [`BigInteger`][big-integer]

As can be seen, there are many signed/unsigned data type combinations (e.g. `int`/`uint` and `long`/`ulong`]. In practice, the unsigned data types are used far less often then the signed ones, so we should probably start with teaching the signed versions. The unsigned versions can then be taught later on.

## Collections

- [`Array`][array]
- [`List<T>`][list]
- [`Dictionary<TKey, TValue>`][dictionary]
- [`HashSet<T>`][hashset]
- [`Stack<T>`][stack]
- [`Queue<T>`][queue]
- `IEnumerable<T>`
- Concurrent collections

The non-generic collections (such as `ArrayList`] are a remnant of the C# 1.0 days, and have mostly been superseded by generic variants when generics were introduced in C# 2.0. As a consequence, these non-generic collection types won't be a part of the v3 track.

## Composite

- [`class`][class]
- [`struct`][struct]
- [`ValueTuple`][tuple]
- [`Tuple`][tuple]

Note: these types are essentially a grouping of one or more other types. As such, it is important to have taught the student several of these other, basic types before teaching these composite types.

Note: the `Tuple` type has been superseded by the `ValueTuple` type, which is why we should only be teaching the latter.

## Advanced

- `Span<T>`
- `Memory<T>`
- [`Range`][range]
- `Index`
- [`Nullable<T>`][nullable]
- `event`
- `delegate`

## Contributing

Thanks for wanting to contribute to the C# track! Contributions are very welcome!

To contribute, please find a type for which no reference document has yet been written and submit a PR to add the missing document.

[bool]: ../../types/boolean.md
[string]: ../../types/string.md
[char]: ../../types/char.md
[null]: ../../types/null.md
[int]: ../../types/integer.md
[uint]: ../../types/integer.md
[byte]: ../../types/byte.md
[sbyte]: ../../types/byte.md
[short]: ../../types/short.md
[ushort]: ../../types/short.md
[long]: ../../types/long.md
[ulong]: ../../types/long.md
[double]: ../../types/double.md
[float]: ../../types/single.md
[decimal]: ../../types/decimal_number.md
[big-integer]: ../../types/big_integer.md
[array]: ../../types/array.md
[list]: ../../types/list.md
[dictionary]: ../../types/map.md
[hashset]: ../../types/set.md
[stack]: ../../types/stack.md
[queue]: ../../types/deque.md
[class]: ../../types/class.md
[struct]: ../../types/struct.md
[value-tuple]: ../../types/tuple.md
[tuple]: ../../types/tuple.md
[range]: ../../types/range.md
[nullable]: ../../types/nullable.md
