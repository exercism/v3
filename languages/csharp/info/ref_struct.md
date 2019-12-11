# Ref struct

A `ref struct` is a struct that will always be allocated on the stack. It has the following properties:

- Represents a sequential struct layout
- Can be used stack-only. i.e. method parameters and local variables
- Cannot be static or instance members of a class or normal struct
- Cannot be method parameter of async methods or lambda expressions
- Cannot be dynamic binding, boxing, unboxing, wrapping or converting
