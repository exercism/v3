_Note: All subconcepts should be split out into separate files under the concepts directory and hyperlinked._

# C&#35;

C# is an object-oriented language.

## Concepts

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

- [Assemblies](../../tooling/dotnet-assemblies.md)

### Memory management

- Resource passing
- Resource allocation
- Resource cleanup
- Resource lifetime

### Arithmetic

- Arithmetic overflow
- Signed integers
- Unsigned integers
- Floating point numbers

### Methods

- Method overloading
- Named arguments
- Optional arguments
- Extension methods (mixin)

### General

- [Sameness](../../../reference/concepts/sameness.md)
- [Conditionals](../../../reference/concepts/conditionals.md)
- [Enumeration](../../../reference/concepts/enumeration.md)
- Iterators (yield)
- Namespaces
- [Generics](../../../reference/concepts/generics.md)
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

### Object-oriented

- [Encapsulation](../../../reference/concepts/encapsulation.md)
  - [Classes](../../../reference/concepts/classes.md)
- [Classes](../../../reference/concepts/classes.md)
- [Objects](../../../reference/concepts/objects.md)
  - [Classes](../../../reference/concepts/classes.md)
- [State](../../../reference/concepts/state.md)
  - [Classes](../../../reference/concepts/classes.md)
- [Mutation](../../../reference/concepts/mutation.md)
- [Composition](../../../reference/concepts/composition.md)
  - [Classes](../../../reference/concepts/classes.md)
  - [Objects](../../../reference/concepts/objects.md)
- [Inheritance](../../../reference/concepts/inheritance.md)
  - [Classes](../../../reference/concepts/classes.md)
- [Interfaces](../../../reference/concepts/interfaces.md)
  - [Classes](../../../reference/concepts/classes.md)
- [Polymorphism](../../../reference/concepts/polymorphism.md)
  - [Inheritance](../../../reference/concepts/inheritance.md)
- [Methods](../../../reference/concepts/methods.md)
  - [Classes](../../../reference/concepts/classes.md)

### Functional

- [Immutability](../../../reference/concepts/immutability.md)
  - [Mutation](../../../reference/concepts/mutation.md)
- [Pattern matching](../../../reference/concepts/pattern_matching.md)
  - [Conditionals](../../../reference/concepts/conditionals.md)
- [Recursion](../../../reference/concepts/recursion.md)
- [Local functions](../../../reference/concepts/nested_functions.md)
  - [Classes](../../../reference/concepts/classes.md)
- [Pipelines (LINQ)](../../../reference/concepts/pipelines.md)
  - [Higher-order functions](../../../reference/concepts/higher_order_functions.md)
  - [Type inference](../../../reference/concepts/type_inference.md)
  - [Anonymous methods](../../../reference/concepts/anonymous_functions.md)
  - Anonymous types
- [Higher-order functions](../../../reference/concepts/higher_order_functions.md)
- [Type inference](../../../reference/concepts/type_inference.md)
- [Anonymous methods](../../../reference/concepts/anonymous_functions.md)
  - [Higher-order functions](../../../reference/concepts/higher_order_functions.md)

### Platform-specific

- [Assemblies](../../tooling/dotnet-assemblies.md)

### Memory management

- Resource passing
  - [Objects](../../../reference/concepts/objects.md)
- Resource allocation
  - [Classes](../../../reference/concepts/classes.md)
  - [Objects](../../../reference/concepts/objects.md)
  - Resource lifetime (static/non-static)
- Resource cleanup
  - [Objects](../../../reference/concepts/objects.md)
  - Resource lifetime (static/non-static)
- Resource lifetime (static/non-static)
  - [Classes](../../../reference/concepts/classes.md)
  - [Objects](../../../reference/concepts/objects.md)

### Arithmetic

- Arithmetic overflow
  - Unsigned integers
- Signed integers
- Unsigned integers
  - Signed integers
- Floating point numbers

### Methods

- Method overloading
  - [Classes](../../../reference/concepts/classes.md)
- Named arguments
- Optional arguments
- Extension methods (mixin)
  - Resource lifetime (static/non-static)

### General

- [Sameness](../../../reference/concepts/sameness.md)
- [Conditionals](../../../reference/concepts/conditionals.md)
  - [Sameness](../../../reference/concepts/sameness.md)
- [Enumeration](../../../reference/concepts/enumeration.md)
- Iterators (yield)
  - [Enumeration](../../../reference/concepts/enumeration.md)
- Namespaces
- [Generics](../../../reference/concepts/generics.md)
- Exception handling
- Type casting (includes boxing/unboxing)
- Anonymous types
  - [Classes](../../../reference/concepts/classes.md)
  - [Anonymous methods](../../../reference/concepts/anonymous_functions.md)
- Concurrency (includes locking and concurrent collections)
- Asynchronous programming
- Expressions vs statements
- Attributes
- Slicing
- Locks
- Unsafe code
- Implicit/explicit conversion
- Reflection

## Types

- [bool](../../../reference/types/boolean.md)
- [string](../../../reference/types/string.md)
- [char](../../../reference/types/char.md)
- object
- Enum
- [Null](../../../reference/types/null.md)
- [Nullable type](../../../reference/types/nullable.md)
- `Span<T>`
- `Memory<T>`
- [Range](../../../reference/types/range.md)
- Event
- Delegate

### Numeric

- [int](../../../reference/types/integer.md)
- [uint](../../../reference/types/integer.md)
- [byte](../../../reference/types/byte.md)
- [sbyte](../../../reference/types/byte.md)
- [short](../../../reference/types/short.md)
- [ushort](../../../reference/types/short.md)
- [long](../../../reference/types/long.md)
- [ulong](../../../reference/types/long.md)
- [double](../../../reference/types/double.md)
- [float](../../../reference/types/single.md)
- [decimal](../../../reference/types/decimal_number.md)
- [BigInteger](../../../reference/types/big_integer.md)

Note: as can be seen, there are many signed/unsigned data type combinations (e.g. `int`/`uint` and `long`/`ulong`). In practice, the unsigned data types are used far less often then the signed ones, so we should probably start with teaching the signed versions. The unsigned versions can then be taught later on.

### Collections

- [Array](../../../reference/types/array.md)
- [List](../../../reference/types/list.md)
- [Dictionary](../../../reference/types/map.md)
- [HashSet](../../../reference/types/set.md)
- [Stack](../../../reference/types/stack.md)
- [Queue](../../../reference/types/deque.md)
- Enumerable
- ConcurrentDictionary (and other concurrent )
- Non-generic collections

Note: The non-generic collections (such as `ArrayList`) are a remnant of the C# 1.0 days, and have been superseded by generic variants when generics were introduced in C# 2.0. As nobody uses the non-generic collections anymore, we'll ignore them.

### Composite

- [Class](../../../reference/types/class.md)
- [Struct](../../../reference/types/struct.md)
- [ValueTuple](../../../reference/types/tuple.md)
- [Tuple](../../../reference/types/tuple.md)

Note: these types are essentially a grouping of one or more other types. As such, it is important to have taught the student several of these other, basic types before teaching these composite types.

Note: the `Tuple` type has been superseded by the `ValueTuple` type, which is why we should only be teaching the latter.

## Resources

- Official language reference: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/
- https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/index
- https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/built-in-types-table
