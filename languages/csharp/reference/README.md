# C&#35; reference

## Concepts

The C# concept exercises are based on concepts. The list below contains the concepts that have been identified for the C# language.

### Object-oriented

The core OO features a C# developer should know about are:

- [Encapsulation](../../../reference/concepts/encapsulation.md)
- [Classes](../../../reference/concepts/classes.md)
- [Objects](../../../reference/concepts/objects.md)
- [State](../../../reference/concepts/state.md)
- [Mutation](../../../reference/concepts/mutation.md)
- [Composition](../../../reference/concepts/composition.md)
- [Inheritance](../../../reference/concepts/inheritance.md)
- [Interfaces](../../../reference/concepts/interfaces.md)
- [Polymorphism](../../../reference/concepts/polymorphism.md)
- [Methods](../../../reference/concepts/methods.md)

### Functional

While C# is primarily an OO language, lots of functional concepts have been added to the language:

- [Immutability](../../../reference/concepts/immutability.md)
- [Pattern matching](../../../reference/concepts/pattern_matching.md)
- [Higher-order functions](../../../reference/concepts/higher_order_functions.md)
- [Type inference](../../../reference/concepts/type_inference.md)
- [Anonymous methods](../../../reference/concepts/anonymous_functions.md)
- [Recursion](../../../reference/concepts/recursion.md)
- [Local functions](../../../reference/concepts/nested_functions.md)
- [Pipelines (LINQ)](../../../reference/concepts/pipelines.md)

### Platform-specific

- [Assemblies](../../../reference/tooling/dotnet-assemblies.md)

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
- [Sameness](../../../reference/concepts/sameness.md)
- [Conditionals](../../../reference/concepts/conditionals.md)
- [Enumeration](../../../reference/concepts/enumeration.md)
- Iterators (yield)
- Namespaces
- [Generics](../../../reference/concepts/generics.md)
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
- Floating point numbers
- [Strings][string]
- Enums
- [Characters][char]
- [Booleans][bool]
- [Arrays][array]
- [Lists][list]
- [Dictionaries][map]
- [Sets][set]
- [Queues][queue]
- [Stacks][stack]
- [Tuples][tuple]
- [Ranges][range]
- Indexes
- Events

## Concept interpretation

The concept exercises use the following concepts:

| concept                   | interpretation                                                                                                                                                                                                                                            |
| ------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `numbers-basic`           | Know of the existence of the two most commonly used number types, `int` and `double`, and understand that the former represents whole numbers, and the latter floating-point numbers. Know of basic operators such as multiplication and comparison.      |
| `numbers-floating-point`  | Know of the existing of the three floating point types: `double`, `float` and `decimal`. Know when to use which type.                                                                                                                                     |
| `strings-basic`           | Know of the existence of the `string` type. Know of some basic functions (like looking up a character at a position, or slicing the string). Know how to do basic string formatting.                                                                      |
| `dates-basic`             | Know of the existence of the `DateTime` type. Know of the individual, date-related properties. Know how to access the current date. Know how to compare dates. Know how to convert a `string` to a `DateTime` and vice versa.                             |
| `enums-basic`             | Know of the existence of the `enum` keyword. Know how to define enum members. Know how to assign values to enum members. Know how to get an enum's numeric value. Know how to convert a `string` to an `enum` and vice versa.                             |
| `enums-advanced`          | Know how to define a "flags" enum. Know how to add, remove or check for flags. Know how to change the underlying type of an enum.                                                                                                                         |
| `time-basic`              | Know of the existence of the `DateTime` type. Know of the individual, time-related properties.                                                                                                                                                            |
| `type-conversion-numbers` | Know how to convert from one numeric type to another. Know what implicit and explicit conversions are.                                                                                                                                                    |
| `conditionals-if`         | Know of the existence of the `if` conditional execution statement.                                                                                                                                                                                        |
| `conditionals-ternary`    | Know of the existence of the ternary operator. Know when to use the ternary operator.                                                                                                                                                                     |
| `loops-while`             | Know how to write a `while` loop.                                                                                                                                                                                                                         |
| `bitwise-operations`      | Know how to apply bitwise operations to numbers. Know where it's documented, or at least how to search for it.                                                                                                                                            |
| `collections-basic`       | Know how to iterate over a collection.                                                                                                                                                                                                                    |
| `attributes-basic`        | Know what attributes are. Know how to annotate code with attributes. Know how to pass properties to attributes.                                                                                                                                           |
| `attributes-advanced`     | Know of the existence of the `Attribute` type. Know what attributes are for. Know how to define custom attributes. Know how to read attribute values at runtime. Know how to limit attribute usage.                                                       |
| `arrays-basic`            | Know of the existence of the `Array` type. Know how to define an array. Know how to access elements in an array by index. Know how to iterate over elements in an array. Know of some basic functions (like finding the index of an element in an array). |

This also indicates that for example `strings-basic` does **not** teach using custom formatting strings and that `numbers-basic` does **not** teach about checked/unchecked arithmetic.

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
[bool]: ../../../reference/types/boolean.md
[string]: ../../../reference/types/string.md
[char]: ../../../reference/types/char.md
[null]: ../../../reference/types/null.md
[int]: ../../../reference/types/integer.md
[uint]: ../../../reference/types/integer.md
[byte]: ../../../reference/types/byte.md
[sbyte]: ../../../reference/types/byte.md
[short]: ../../../reference/types/short.md
[ushort]: ../../../reference/types/short.md
[long]: ../../../reference/types/long.md
[ulong]: ../../../reference/types/long.md
[double]: ../../../reference/types/double.md
[float]: ../../../reference/types/single.md
[decimal]: ../../../reference/types/decimal_number.md
[big-integer]: ../../../reference/types/big_integer.md
[array]: ../../../reference/types/array.md
[list]: ../../../reference/types/list.md
[map]: ../../../reference/types/map.md
[set]: ../../../reference/types/set.md
[stack]: ../../../reference/types/stack.md
[queue]: ../../../reference/types/deque.md
[class]: ../../../reference/types/class.md
[struct]: ../../../reference/types/struct.md
[value-tuple]: ../../../reference/types/tuple.md
[tuple]: ../../../reference/types/tuple.md
[range]: ../../../reference/types/range.md
[nullable]: ../../../reference/types/nullable.md
