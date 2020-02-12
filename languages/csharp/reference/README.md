# C&#35; reference

## Concepts

The C# concept exercises are based on concepts. The list below contains the concepts that have been identified for the C# language.

### Object-oriented

The core OO features a C# developer should know about are:

- [Classes][classes]
- Structs
- [State][state]
- [Mutation][mutation]
- [Objects][objects]
  - Object initializers
- [Encapsulation][encapsulation]
- [Composition][composition]
- [Inheritance][inheritance]
- [Interfaces][interfaces]
- [Polymorphism][polymorphism]
- Statics
- [Methods][methods]
  - Constructors
  - Destructors
  - Method overloading
  - Extension methods
  - Return values
  - Method arguments
    - Named arguments
    - [Optional arguments][optional_arguments]
    - Out parameters
- Fields
- Properties
- Indexers

### Functional

While C# is primarily an OO language, lots of functional concepts have been added to the language:

- [Functions][functions]
  - [Higher-order functions][higher_order_functions]
  - [Anonymous functions][anonymous_functions]
  - [Local functions][local_functions]
- [Immutability][immutability]
- [Pattern matching][pattern_matching]
- [Type inference][type_inference]
- [Recursion][recursion]
- [LINQ][linq]
- Expressions vs statements

### General

- [Variables]
  - Assignment
  - Default values (a `bool` being `false` by default, etc.)
- Scoping
  - Namespaces
  - Imports (usings)
  - Visibility (`public`, `private`, etc.)
- Numbers
  - Math operators
  - Bitwise manipulation
  - Arithmetic overflow
- Comparison
  - [Equality][equality] (`Equals`, `GetHashCode`)
  - Ordering
- Conditionals
  - Boolean logic
  - [Conditionals: if][conditionals]
  - [Conditionals: ternary][conditionals]
  - Conditionals: while
  - Conditionals: do-while
  - Conditionals: switch
- Enumeration
  - [Enumeration: for loop][enumeration]
  - [Enumeration: foreach loop][enumeration]
- Collections
  - [Generics][generics]
  - Iterators (yield)
  - Collections: mapping
  - Collections: filtering
  - Collections: reducing
  - Collections: ordering
  - Collections: combining
- Resources
  - Resource passing (by reference/by value)
  - [Resource allocation][memory_allocation]
  - Resource cleanup (`IDisposable`)
  - Resource lifetime
- Conversions
  - Implicit
  - Explicit (casts)
  - Boxing/unboxing
- Null
  - Nullable values
  - Null-coalescing operator
  - Null-conditional operator
- Concurrency
  - Concurrent collections
  - Locks
- Exceptions
- Asynchronous programming
- Attributes
- Slicing
- Unsafe code
- Comments
- Randomness
- Regular expressions
- Anonymous types
- Nested types
- String interpolation

### Types

- [Strings][string]
- [Characters][char]
- [Booleans][bool]
- Enums
- [Tuples][tuple]
- Numbers
  - Signed integers
  - Unsigned integers
  - Floating point numbers
- Collections
  - [Arrays][array]
  - [Lists][list]
  - [Dictionaries][map]
  - [Sets][set]
  - [Queues][queue]
  - [Stacks][stack]
  - [Ranges][range]
  - Enumerables
  - Immutable collections
- Indexes
- Events
- Delegates
- Tasks

## Concept interpretation

The concept exercises use the following concepts:

| concept                   | interpretation                                                                                                                                                                                                                                                                                 |
| ------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `numbers-basic`           | Know of the existence of the two most commonly used number types, `int` and `double`, and understand that the former represents whole numbers, and the latter floating-point numbers. Know of basic operators such as multiplication and comparison.                                           |
| `numbers-floating-point`  | Know of the existing of the three floating point types: `double`, `float` and `decimal`. Know when to use which type.                                                                                                                                                                          |
| `strings-basic`           | Know of the existence of the `string` type. Know how to create a string. Know of some basic methods (like finding the index of a character in a string, or returning a part the string). Know how to do basic string formatting. Know where it's documented, or at least how to search for it. |
| `dates-basic`             | Know of the existence of the `DateTime` type. Know of the individual, date-related properties. Know how to access the current date. Know how to compare dates. Know how to convert a `string` to a `DateTime` and vice versa.                                                                  |
| `enums-basic`             | Know of the existence of the `enum` keyword. Know how to define enum members. Know how to assign values to enum members. Know how to get an enum's numeric value. Know how to convert a `string` to an `enum` and vice versa.                                                                  |
| `enums-advanced`          | Know how to define a "flags" enum. Know how to add, remove or check for flags. Know how to change the underlying type of an enum.                                                                                                                                                              |
| `time-basic`              | Know of the existence of the `DateTime` type. Know of the individual, time-related properties.                                                                                                                                                                                                 |
| `type-conversion-numbers` | Know how to convert from one numeric type to another. Know what implicit and explicit conversions are.                                                                                                                                                                                         |
| `conditionals-if`         | Know of the existence of the `if` conditional execution statement.                                                                                                                                                                                                                             |
| `conditionals-ternary`    | Know of the existence of the ternary operator. Know when to use the ternary operator.                                                                                                                                                                                                          |
| `loops-while`             | Know how to write a `while` loop.                                                                                                                                                                                                                                                              |
| `bitwise-operations`      | Know how to apply bitwise operations to numbers. Know where it's documented, or at least how to search for it.                                                                                                                                                                                 |
| `collections-basic`       | Know how to iterate over a collection.                                                                                                                                                                                                                                                         |
| `attributes-basic`        | Know what attributes are. Know how to annotate code with attributes. Know how to pass properties to attributes.                                                                                                                                                                                |
| `attributes-advanced`     | Know of the existence of the `Attribute` type. Know what attributes are for. Know how to define custom attributes. Know how to read attribute values at runtime. Know how to limit attribute usage.                                                                                            |
| `arrays-basic`            | Know of the existence of the `Array` type. Know how to define an array. Know how to access elements in an array by index. Know how to iterate over elements in an array. Know of some basic functions (like finding the index of an element in an array).                                      |

This also indicates that for example `strings-basic` does **not** teach using custom formatting strings and that `numbers-basic` does **not** teach about checked/unchecked arithmetic.

## Reference docs

Reference docs are written to help explain a particular C# concept to a student when no appropriate online document can be found. They will be used when creating exercises and as references in exercise documentation.

The following reference docs have been written:

- [Code style][code_style]
- [Memory allocation][memory_allocation]

The following reference docs should be written:

- Reference doc on difference between `static readonly` and `const`
- Naming conventions

[issues-improve-reference]: https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%3Atrack%2Fcsharp+label%3Atype%2Fimprove-reference+label%3Astatus%2Fhelp-wanted
[issues-new-reference]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Atrack%2Fcsharp+label%3Atype%2Fnew-reference+label%3Astatus%2Fhelp-wanted+
[code_style]: ./code_style.md
[memory_allocation]: ./memory_allocation.md
[encapsulation]: ../../../reference/concepts/encapsulation.md
[classes]: ../../../reference/concepts/classes.md
[objects]: ../../../reference/concepts/objects.md
[state]: ../../../reference/concepts/state.md
[mutation]: ../../../reference/concepts/mutation.md
[composition]: ../../../reference/concepts/composition.md
[inheritance]: ../../../reference/concepts/inheritance.md
[interfaces]: ../../../reference/concepts/interfaces.md
[polymorphism]: ../../../reference/concepts/polymorphism.md
[methods]: ../../../reference/concepts/methods.md
[immutability]: ../../../reference/concepts/immutability.md
[pattern_matching]: ../../../reference/concepts/pattern_matching.md
[higher_order_functions]: ../../../reference/concepts/higher_order_functions.md
[type_inference]: ../../../reference/concepts/type_inference.md
[anonymous_functions]: ../../../reference/concepts/anonymous_functions.md
[recursion]: ../../../reference/concepts/recursion.md
[local_functions]: ../../../reference/concepts/nested_functions.md
[linq]: ../../../reference/concepts/pipelines.md
[equality]: ../../../reference/concepts/sameness.md
[conditionals]: ../../../reference/concepts/conditionals.md
[enumeration]: ../../../reference/concepts/enumeration.md
[generics]: ../../../reference/concepts/generics.md
[assemblies]: ../../../reference/tooling/dotnet-assemblies.md
[bool]: ../../../reference/types/boolean.md
[string]: ../../../reference/types/string.md
[char]: ../../../reference/types/char.md
[null]: ../../../reference/types/null.md
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
[nullable]: ../../../reference/types/nullable.md
[optional_arguments]: ../../../reference/concepts/default_arguments.md
[functions]: ../../../reference/types/function.md
[variables]: ../../../reference/concepts/variables.md
