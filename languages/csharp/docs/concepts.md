# C&#35; concepts

The [C# concept exercises][exercises-concept] are all based on [concepts][docs-concept]. The list below contains the concepts that have been identified for the C# language. These concepts will be [converted to concepted exercises][docs-how-to-implement-a-concept-exercise].

## Object-oriented

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

## Functional

While C# is primarily an OO language, lots of functional concepts have been added to the language:

- [Immutability](../../concepts/immutability.md)
- [Pattern matching](../../concepts/pattern_matching.md)
- [Higher-order functions](../../concepts/higher_order_functions.md)
- [Type inference](../../concepts/type_inference.md)
- [Anonymous methods](../../concepts/anonymous_functions.md)
- [Recursion](../../concepts/recursion.md)
- [Local functions](../../concepts/nested_functions.md)
- [Pipelines (LINQ)](../../concepts/pipelines.md)

## Platform-specific

- [Assemblies](../../tooling/dotnet-assemblies.md)

## Memory management

- Resource passing
- [Resource allocation](./memory_allocation.md) (can include `Span<T>`/`Memory<T>` types)
- Resource cleanup
- Resource lifetime

## Methods

- Method overloading
- Named arguments
- Optional arguments
- Extension methods (mixin)

## General

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

## Types

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

## Contributing

Thanks for wanting to contribute to the C# track! Contributions are very welcome!

To contribute, please find a concept for which no reference document has yet been written and submit a PR to add the missing document. Some concepts currently link to the general concept description, but could benefit from having a C#-specific reference document. PR's to add these are also greatly appreciated.

[exercises-concept]: ../../exercises/concept/README.md
[docs-concept]: ../../../../docs/concept-exercises.md
[docs-how-to-implement-a-concept-exercise]: ../../docs/how-to-implement-a-concept-exercise.md
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
