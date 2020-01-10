# C&#35;

Welcome to the workspace for Exercism v3's C# track!

The Exercism v3 C# track helps students learn C#. It does this through a series of carefully designed Concept Exercises and Practice Exercises. This repository contains all those exercises, as well as reference documentation that help explain C# concepts for which no appropriate online document could be found.

## Contributing

The C# maintainers have written out specs for lots of exercises that need writing. You can help us by choosing an exercise and implementing it. You can find the list of exercises [here][github-issues-new-exercise] - each one has detailed instructions on what to do.

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

## FAQs

- [What is Exercism v3?][exercism-v3]

[exercism-v3]: ../../README.md
[reference]: ./reference/README.md
[repository-structure]: ./docs/repository-structure.md
[github-issues-new-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Atrack%2Fcsharp+label%3Atype%2Fnew-exercise+label%3Astatus%2Fhelp-wanted+
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
