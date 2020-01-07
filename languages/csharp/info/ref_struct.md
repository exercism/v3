# Ref struct

A `ref struct` is a [struct][csharp-types-struct] that will always be [allocated on the stack][csharp-info-memory_allocation]. It has the following properties:

- Represents a sequential struct layout.
- Can be used stack-only. i.e. method parameters and local variables.
- Cannot be static or instance members of a class or normal struct.
- Cannot be method parameter of async methods or lambda expressions.
- Cannot be dynamic binding, boxing, unboxing, wrapping or converting.

[csharp-types-struct]: ../../reference/types/struct.md
[csharp-info-memory_allocation]: ./memory_allocation.md
