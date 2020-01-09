# `Struct`

A `struct` type is a [value type][csharp-info-value_types] that is typically used to encapsulate small groups of related variables.

All simple types are structs:

- Integral types: integer numeric types and the [`char` type][csharp-type-char]
- [Floating-point types][csharp-info-floating_point_numbers]
- [`bool`][csharp-type-bool]

## Ref struct

A `ref struct` is a special type of struct that will always be [allocated on the stack][csharp-info-memory_allocation]. It has the following properties:

- Represents a sequential struct layout.
- Can be used stack-only. i.e. method parameters and local variables.
- Cannot be static or instance members of a class or normal struct.
- Cannot be method parameter of async methods or lambda expressions.
- Cannot be dynamic binding, boxing, unboxing, wrapping or converting.

[csharp-info-value_types]: ../concepts/value_types.md
[csharp-info-floating_point_numbers]: ../concepts/floating_point_numbers.md
[csharp-info-memory_allocation]: ../concepts/memory_allocation.md
[csharp-type-bool]: ./bool.md
[csharp-type-char]: ./char.md
