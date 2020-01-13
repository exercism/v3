# C&#35; reference

## Concepts

The C# concept exercises are based on concepts. The list below contains the concepts that have been identified for the C# language.

### Object-oriented

The core OO features a C# developer should know about are:

- [Encapsulation](../../concepts/encapsulation.md)
- [Classes](../../concepts/classes.md)
- [Objects](../../concepts/objects.md)
- [State](../../concepts/state.md)
- [Mutation](../../concepts/mutation.md)
- [Composition](../../concepts/composition.md)
- [Inheritance](../../concepts/inheritance.md)
- [Interfaces](../../concepts/interfaces.md)
- [Polymorphism](../../concepts/polymorphism.md)
- [Methods](../../concepts/methods.md)

### Functional

While C# is primarily an OO language, lots of functional concepts have been added to the language:

- [Immutability](../../concepts/immutability.md)
- [Pattern matching](../../concepts/pattern_matching.md)
- [Higher-order functions](../../concepts/higher_order_functions.md)
- [Type inference](../../concepts/type_inference.md)
- [Anonymous methods](../../concepts/anonymous_functions.md)
- [Recursion](../../concepts/recursion.md)
- [Local functions](../../concepts/nested_functions.md)
- [Pipelines (LINQ)](../../concepts/pipelines.md)

### Platform-specific

- [Assemblies](../../tooling/dotnet-assemblies.md)

### Memory management

- Resource passing
- [Resource allocation](./memory_allocation.md) (can include `Span<T>`/`Memory<T>` types)
- Resource cleanup
- Resource lifetime

### Methods

- Method overloading
- Named arguments
- Optional arguments
- Extension methods (mixin)

### General

- Arithmetic overflow
- [Sameness](../../concepts/sameness.md)
- [Conditionals](../../concepts/conditionals.md)
- [Enumeration](../../concepts/enumeration.md)
- Iterators (yield)
- Namespaces
- [Generics](../../concepts/generics.md)
- Exception handling
- Implicit/explicit conversion
- Boxing/unboxing
- Anonymous types
- Concurrency (can include concurrent collections)
- Asynchronous programming
- Expressions vs statements
- Attributes
- Slicing
- Unsafe code
- Reflection
- Nullability

### Types

- Signed integers
- Unsigned integers
- [Floating point numbers](./floating_point_numbers.md)
- Strings
- Enums
- Characters
- [Booleans][bool]
- [Arrays][array]
- [Lists][list]
- [Dictionaries][map]
- [Sets][set]
- Queues
- Stacks
- Tuples
- Ranges
- Indexes
- Events

## Concept interpretation

The concept exercises use the following concepts:

| concept                  | interpretation                                                                                                                                                                                                                                                                                                     |
| ------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `basic-numbers`          | Know of the existence of the two most commonly used number types, `int` and `double`, and understand that the former represents whole numbers, and the latter floating-point numbers. Know of basic operators such as multiplication and comparison. Know where it's documented, or at least how to search for it. |
| `basic-strings`          | Know of the existence of the `string` type. Know of some basic functions (like looking up a character at a position, or slicing the string). Know how to do basic string formatting. Know where it's documented, or at least how to search for it.                                                                 |
| `basic-dates`            | Know of the existence of the `DateTime` type. Know of the individual, date-related properties. Know how to access the current date. Know how to compare dates. Know how to convert a `string` to a `DateTime` and vice versa. Know where it's documented, or at least how to search for it                         |
| `basic-enums`            | Know of the existence of the `enum` keyword. Know how to define enum members. Know how to convert a `string` to an `enum` and vice versa. Know where it's documented, or at least how to search for it                                                                                                             |
| `advanced-enums`         | Know how to define a "flags" enum. Know how to add, remove or check for flags.                                                                                                                                                                                                                                     |
| `basic-time`             | Know of the existence of the `DateTime` type. Know of the individual, time-related properties. Know where it's documented, or at least how to search for it.                                                                                                                                                       |
| `basic-type-conversion`  | Know that it is sometimes possible to convert from one type to another type.                                                                                                                                                                                                                                       |
| `conditionals`           | Know of the existence of conditional execution statements (such as the `if` statement).                                                                                                                                                                                                                            |
| `floating-point-numbers` | Know of the existing of the three floating point types: `double`, `float` and `decimal`. Know when to use which type.                                                                                                                                                                                              |
| `string-formatting`      | Know how to format a string. Know where it's documented, or at least how to search for it.                                                                                                                                                                                                                         |
| `bitwise-operations`     | Know how to apply bitwise operations to numbers. Know where it's documented, or at least how to search for it.                                                                                                                                                                                                     |

This also indicates that for example `basic-strings` does **not** teach using custom formatting strings and that `basic-numbers` does **not** teach about checked/unchecked arithmetic.

## Reference docs

Reference docs are written to help explain a particular C# concept to a student when no appropriate online document can be found. They will be used when creating exercises and as references in exercise documentation.

The following reference docs have been written:

- [Code style][code_style]
- [Memory allocation][memory_allocation]

## TODO

- Reference doc on difference between `static readonly` and `const`

[issues-improve-reference]: https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%3Atrack%2Fcsharp+label%3Atype%2Fimprove-reference+label%3Astatus%2Fhelp-wanted
[issues-new-reference]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Atrack%2Fcsharp+label%3Atype%2Fnew-reference+label%3Astatus%2Fhelp-wanted+
[code_style]: ./code_style.md
[memory_allocation]: ./memory_allocation.md
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
[map]: ../../types/map.md
[set]: ../../types/set.md
[stack]: ../../types/stack.md
[queue]: ../../types/deque.md
[class]: ../../types/class.md
[struct]: ../../types/struct.md
[value-tuple]: ../../types/tuple.md
[tuple]: ../../types/tuple.md
[range]: ../../types/range.md
[nullable]: ../../types/nullable.md
