# C++ reference

## Concepts

The C++ concept exercises are based on concepts. The list below contains the concepts that have been identified for the C++ language.

### Object-oriented

The core OO features a C++ developer should know about are:

- Initialization
- [Encapsulation](../../../reference/concepts/encapsulation.md)
- [Classes](../../../reference/concepts/classes.md)
- [Objects](../../../reference/concepts/objects.md)
- [State](../../../reference/concepts/state.md)
- [Mutation](../../../reference/concepts/mutation.md)
- [Composition](../../../reference/concepts/composition.md)
- [Inheritance](../../../reference/concepts/inheritance.md)
- [Interfaces](../../../reference/concepts/interfaces.md)
- [Runtime polymorphism](../../../reference/concepts/polymorphism.md)
- [Methods](../../../reference/concepts/methods.md)

### Functional

C++ is a very flexible language, so lots of functional concepts apply to it:

- [Immutability](../../../reference/concepts/immutability.md) (`const`ness)
- [Higher-order functions](../../../reference/concepts/higher_order_functions.md)
- [Type inference](../../../reference/concepts/type_inference.md) (`auto`, FTAD, and CTAD)
- [Anonymous functions (lambdas)](../../../reference/concepts/anonymous_functions.md)
- [Recursion](../../../reference/concepts/recursion.md)
- [Nested functions](../../../reference/concepts/nested_functions.md)
- [Pipelines (ranges)](../../../reference/concepts/pipelines.md) (Defer C++2a to release)

### Memory management

- Object lifetime
- RAII
- Move-semantics
- Free-store (heap)

### Functions

- Function overloading
- Default parameters
- Special member-functions
- [Operators](../../../reference/concepts/operators.md)

### General

- [Arithmetic](../../../reference/concepts/arithmetic.md)
- [Sameness](../../../reference/concepts/sameness.md) (Equality)
- Ordering
- [Conditionals](../../../reference/concepts/conditionals.md)
- [Enumeration](../../../reference/concepts/enumeration.md) (Container iteration)
- Iterators
- Namespaces
- C-style Input/Output
- Input/Output stream-based library
- Templates
- [Metaprogramming](../../../reference/concepts/metaprogramming.md)
- Exception handling
- Implicit/explicit/contextual conversion
- [Destructuring (structured-bindings)](../../../reference/concepts/destructuring_assignment.md)
- Concurrency (thread-safety)
- Expressions vs statements
- Value categories
- Attributes
- Slicing
- String literals
- Reflection (`<type_traits>`)
- Undefined Behavior
- Preprocessor
- Concepts (Defer to C++2a)
- Formatting (Defer to C++2a)

### Types

- Signed integers
- Unsigned integers
- Fixed width integer types
- Floating point numbers
- [Characters][char]
- [Booleans][bool]
- Enums
- [Strings][string]
- [Arrays][array]
- Vectors
- Views (`std::span` and `std::string_view`)
- Reference types
- [Linked-lists][list]
- [Dictionaries][map]
- [Sets][set]
- [Queues][queue]
- [Stacks][stack]
- [Tuples][tuple]
- Sum types (`std::variant`)
- Product types (`std::pair`, `std::tuple`)
- [Ranges][range] (Defer to C++2a)
- Bitset
- Dynamic bitset

### Advanced topics

- Pointers
- Unions (`union`)

## Concept interpretation

The concept exercises use the following concepts:

| concept                  | minimum standard version | interpretation                                                                                                                                                                                                                                                                                                               |
| ------------------------ | ------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `numbers-basic`          | C++98                    | Know of the existence of the two most basic number types, `int` and `double`, and understand that the former represents whole numbers, and the latter floating-point numbers. Know of basic operators such as multiplication and comparison. Know where it's documented, or at least how to search for it.                   |
| `numbers-integral`       | C++98                    | Know of the existence of the integer types: `int`, `long`, `long long` (with unsigned and fixed-width variants). Know when to use which type.                                                                                                                                                                                |
| `numbers-floating-point` | C++98                    | Know of the existence of the three floating point types: `float`, `double` and `long double`. Know when to use which type.                                                                                                                                                                                                   |
| `strings-basic`          | C++98                    | Know of the existence of the `std::string` type. Know of some basic functions (like looking up a character at a position, or slicing the string). Know where it's documented, or at least how to search for it.                                                                                                              |
| `strings-formatting`     | C++2a                    | Know how to format a string. Know where it's documented, or at least how to search for it.                                                                                                                                                                                                                                   |
| `chrono-basic`           | C++11                    | Know of the existence of the `<chrono>` header. Know of the existence of all three standard clocks and when to use them. Know how to access the current time. Know how to compare dates. Know how to convert a string to a chrono time point and vice versa. Know where it's documented, or at least how to search for it.   |
| `enums-basic`            | C++98                    | Know of the existence of the `enum class` keyword. Know how to define enum members. Know how to use an enumerators. Know where it's documented, or at least how to search for it                                                                                                                                             |
| `enums-advanced`         | C++98                    | Know how to define a "flags" enum class. Know how to add, remove or check for flags. Know the difference between a C-style enum and a strongly typed enum                                                                                                                                                                    |
| `casts-basic`            | C++98                    | Know that it is sometimes possible to convert from one type to another type using `static_cast`.                                                                                                                                                                                                                             |
| `conditionals`           | C++98                    | Know of the existence of conditional execution statements (such as the `if` or `switch` statement).                                                                                                                                                                                                                          |
| `conditional-injection`  | C++17                    | Know of the existence of conditional code injection through `if constexpr`.                                                                                                                                                                                                                                                  |
| `bitwise-operations`     | C++98                    | Know how to apply bitwise operations to numbers. Know where it's documented, or at least how to search for it.                                                                                                                                                                                                               |
| `iteration-basic`        | C++11                    | Know how to iterate over a collection (range-`for`).                                                                                                                                                                                                                                                                         |
| `arrays-basic`           | C++11                    | Know of the existence of the `std::array` type. Know how to define an array. Know how to access elements in an array by index. Know how to iterate over elements in an array. Know of some basic functions (like finding the index of an element in an array). Know where it's documented, or at least how to search for it. |
| `vectors-basic`          | C++98                    | Know of the existence of the `std::vector` type. Know how to define an array. Know how to access elements in an vector by index. Know the unique properties of a vector. Know of some basic functions (like adding an element to a vector). Know where it's documented, or at least how to search for it.                    |
| `maps-basic`             | C++98                    | Know of the existence of the `std::map` and `std::unordered_map` types. Know how to define an map. Know how to access elements in an map by key. Know the unique properties of a map. Know of some basic functions (like adding an element to a map). Know where it's documented, or at least how to search for it.          |
| `sets-basic`             | C++98                    | Know of the existence of the `std::set` and `std::unordered_set` types. Know how to define an set. Know how to access elements in a set. Know the unique properties of a set. Know of some basic functions (like adding an element to a set). Know where it's documented, or at least how to search for it.                  |

This also indicates that for example `strings-basic` does **not** teach using custom formatting strings and that `numbers-basic` does **not** teach about integer undefined-behavior.

[issues-improve-reference]: https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%3Atrack%2Fcpp+label%3Atype%2Fimprove-reference+label%3Astatus%2Fhelp-wanted
[issues-new-reference]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Atrack%2Fcpp+label%3Atype%2Fnew-reference+label%3Astatus%2Fhelp-wanted+
[bool]: ../../../reference/types/boolean.md
[string]: ../../../reference/types/string.md
[char]: ../../../reference/types/char.md
[nullptr]: ../../../reference/types/null.md
[int/long]: ../../../reference/types/integer.md
[unsigned long]: ../../../reference/types/integer.md
[short]: ../../../reference/types/short.md
[unsigned short]: ../../../reference/types/short.md
[long]: ../../../reference/types/long.md
[ulong]: ../../../reference/types/long.md
[double]: ../../../reference/types/double.md
[float]: ../../../reference/types/single.md
[array]: ../../../reference/types/array.md
[list]: ../../../reference/types/list.md
[map]: ../../../reference/types/map.md
[set]: ../../../reference/types/set.md
[stack]: ../../../reference/types/stack.md
[queue]: ../../../reference/types/deque.md
[class]: ../../../reference/types/class.md
[struct]: ../../../reference/types/struct.md
[tuple]: ../../../reference/types/tuple.md
[range]: ../../../reference/types/range.md
