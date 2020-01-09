# C&#35; concepts

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
- [Resource allocation](./memory_allocation.md)
- Resource cleanup
- Resource lifetime

## Arithmetic

- Arithmetic overflow
- Signed integers
- Unsigned integers
- [Floating point numbers](./floating_point_numbers.md)

## Methods

- Method overloading
- Named arguments
- Optional arguments
- Extension methods (mixin)

## General

- [Sameness](../../concepts/sameness.md)
- [Conditionals](../../concepts/conditionals.md)
- [Enumeration](../../concepts/enumeration.md)
- Iterators (yield)
- Namespaces
- [Generics](../../concepts/generics.md)
- Exception handling
- Type casting (includes boxing/unboxing)
- Anonymous types
- Concurrency
- Asynchronous programming
- Expressions vs statements
- Attributes
- Slicing
- Unsafe code
- Implicit/explicit conversion
  - Type casting (includes boxing/unboxing)
- Reflection

## Concept dependencies

Some of these dependencies will be dependent on how the actual exercise is implemented. For example, if the "Classes" concept is implemented with methods that use strings, than the "Strings" concept will be a dependency. The current list does not include those implementation details, as the concept exercises have not yet been designed. This means that this list is a work in progress.

The list itself should be read as follows:

- [concept]
  - [concept-prerequisite1]
  - [concept-prerequisite2]
  - ...

This also means that some concepts will be listed more than once.

## Object-oriented

- [Encapsulation](../../concepts/encapsulation.md)
  - [Classes](../../concepts/classes.md)
- [Classes](../../concepts/classes.md)
- [Objects](../../concepts/objects.md)
  - [Classes](../../concepts/classes.md)
- [State](../../concepts/state.md)
  - [Classes](../../concepts/classes.md)
- [Mutation](../../concepts/mutation.md)
- [Composition](../../concepts/composition.md)
  - [Classes](../../concepts/classes.md)
  - [Objects](../../concepts/objects.md)
- [Inheritance](../../concepts/inheritance.md)
  - [Classes](../../concepts/classes.md)
- [Interfaces](../../concepts/interfaces.md)
  - [Classes](../../concepts/classes.md)
- [Polymorphism](../../concepts/polymorphism.md)
  - [Inheritance](../../concepts/inheritance.md)
- [Methods](../../concepts/methods.md)
  - [Classes](../../concepts/classes.md)

## Functional

- [Immutability](../../concepts/immutability.md)
  - [Mutation](../../concepts/mutation.md)
- [Pattern matching](../../concepts/pattern_matching.md)
  - [Conditionals](../../concepts/conditionals.md)
- [Recursion](../../concepts/recursion.md)
- [Local functions](../../concepts/nested_functions.md)
  - [Classes](../../concepts/classes.md)
- [Pipelines (LINQ)](../../concepts/pipelines.md)
  - [Higher-order functions](../../concepts/higher_order_functions.md)
  - [Type inference](../../concepts/type_inference.md)
  - [Anonymous methods](../../concepts/anonymous_functions.md)
  - Anonymous types
- [Higher-order functions](../../concepts/higher_order_functions.md)
- [Type inference](../../concepts/type_inference.md)
- [Anonymous methods](../../concepts/anonymous_functions.md)
  - [Higher-order functions](../../concepts/higher_order_functions.md)

## Platform-specific

- [Assemblies](../../tooling/dotnet-assemblies.md)

## Memory management

- Resource passing
  - [Objects](../../concepts/objects.md)
- Resource allocation
  - [Classes](../../concepts/classes.md)
  - [Objects](../../concepts/objects.md)
  - Resource lifetime (static/non-static)
- Resource cleanup
  - [Objects](../../concepts/objects.md)
  - Resource lifetime (static/non-static)
- Resource lifetime (static/non-static)
  - [Classes](../../concepts/classes.md)
  - [Objects](../../concepts/objects.md)

## Arithmetic

- Arithmetic overflow
  - Unsigned integers
- Signed integers
- Unsigned integers
  - Signed integers
- Floating point numbers

## Methods

- Method overloading
  - [Classes](../../concepts/classes.md)
- Named arguments
- Optional arguments
- Extension methods (mixin)
  - Resource lifetime (static/non-static)

## General

- [Sameness](../../concepts/sameness.md)
- [Conditionals](../../concepts/conditionals.md)
  - [Sameness](../../concepts/sameness.md)
- [Enumeration](../../concepts/enumeration.md)
- Iterators (yield)
  - [Enumeration](../../concepts/enumeration.md)
- Namespaces
- [Generics](../../concepts/generics.md)
- Exception handling
- Type casting (includes boxing/unboxing)
- Anonymous types
  - [Classes](../../concepts/classes.md)
  - [Anonymous methods](../../concepts/anonymous_functions.md)
- Concurrency (includes locking and concurrent collections)
- Asynchronous programming
- Expressions vs statements
- Attributes
- Slicing
- Locks
- Unsafe code
- Implicit/explicit conversion
- Reflection

## Contributing

Thanks for wanting to contribute to the C# track! Contributions are very welcome!

To contribute, please find a concept for which no reference document has yet been written and submit a PR to add the missing document.
