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

## Reference docs

Reference docs are written to help explain a particular C# concept to a student when no appropriate online document can be found. They will be used when creating exercises and as references in exercise documentation.

The following reference docs have been written:

- [Code style][code_style]
- [Memory allocation][memory_allocation]

## Contribute

You can help us by submitting writing reference docs for one of the topics in the following list:

- Tail calls not being optimized
- Difference between `static readonly` and `const`

You can also see if there are any [open issues for reference doc improvements][issues-reference-improve].

[code_style]: ./code_style.md
[memory_allocation]: ./memory_allocation.md
[issues-reference-improve]: https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%3Atrack%2Fcsharp+label%3Atype%2Fimprove-reference+label%3Astatus%2Fhelp-wanted
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
