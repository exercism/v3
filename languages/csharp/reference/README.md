# C&#35; reference

## Concepts

The C# concept exercises are based on concepts. The list below contains the concepts that have been identified for the C# language.

### Object-oriented

The core OO features a C# developer should know about are:

- [Classes][classes]
- [Composition][composition]
- [Encapsulation][encapsulation]
- Fields
- Indexers
- [Inheritance][inheritance]
- [Interfaces][interfaces]
  - Explicit Interface Implementation
- [Methods][methods]
  - Constructors
  - Destructors
  - Extension methods
  - Method arguments
    - Named arguments
    - Out parameters
    - [Optional arguments][optional_arguments]
  - Method overloading
  - Return values
- [Mutation][mutation]
- [Objects][objects]
  - Object initializers
- [Polymorphism][polymorphism]
- Properties
- [State][state]
- Statics
- Structs

### Functional

While C# is primarily an OO language, lots of functional concepts have been added to the language:

- Expression-bodied members
- Expressions vs statements
- [Functions][functions]
  - [Anonymous functions][anonymous_functions]
  - [Higher-order functions][higher_order_functions]
  - [Local functions][local_functions]
- [Immutability][immutability]
- [LINQ][linq]
  - Advanced (GroupBy, Join, Zip, Cast, GroupJoin, AsEnumerable)
  - Method Classification (deferred, non-streaming etc.)
  - Query Syntax
  - SelectMany
- [Pattern matching][pattern_matching]
- [Recursion][recursion]
- [Type inference][type_inference]

### General

- Asynchronous programming
- Attributes
- Collections
  - Collections: combining
  - Collections: filtering
  - Collections: mapping
  - Collections: ordering
  - Collections: reducing
  - Iterators (yield)
    - Async iterators
  - [Generics][generics]
    - Constraints
    - Covariance/Contravariance
- Comments
- Comparison
  - [Equality][equality] (`Equals`, `GetHashCode`)
  - Ordering
- Concurrency
  - Concurrent collections
  - Locks
- Conditionals
  - Boolean logic
  - Conditionals: do-while
  - Conditionals: switch
  - Conditionals: while
  - [Conditionals: if][conditionals]
  - [Conditionals: ternary][conditionals]
- Conversions
  - Boxing/unboxing
  - Explicit (casts)
  - Implicit
- Enumeration
  - [Enumeration: for loop][enumeration]
  - [Enumeration: foreach loop][enumeration]
- Exceptions
- Null
  - Null-coalescing operator
  - Null-conditional operator
  - Null-forgiving operator
  - Nullable values
- Numbers
  - Arithmetic overflow
  - Bitwise manipulation
  - Math operators
- Randomness
- Reflection
- Regular expressions
- Resources
  - Resource cleanup (`IDisposable`)
  - Resource lifetime
  - Resource passing (by reference/by value)
  - [Resource allocation][memory_allocation]
- Scoping
  - Imports (usings)
  - Namespaces
  - Visibility (`public`, `private`, etc.)
- Slicing
- String formatting
  - Formatting types
  - Interpolation
  - StringBuilder
- Unsafe code
- [Variables][variables]
  - Assignment
  - Default values (a `bool` being `false` by default, etc.)

### Types

- Anonymous types
- [Booleans][bool]
- [Characters][char]
- Collections
  - [Arrays][array]
  - [Dictionaries][map]
  - Enumerables
  - Immutable collections
  - [Lists][list]
  - [Queues][queue]
  - [Ranges][range]
  - [Sets][set]
  - [Stacks][stack]
- Dates
  - Time zones
- Delegates
- Enums
- Events
- Indexes
- Lazy&lt;T&gt;
- Nested types
- Numbers
  - Floating point numbers
  - Signed integers
  - Unsigned integers
- Streams
- [Strings][string]
- Tasks
- Time
- [Tuples][tuple]

## Concept interpretation

The concept exercises use the following concepts:

| concept                      | interpretation                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| ---------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `arrays`                     | Know of the existence of the `Array` type. Know how to define an array. Know how to access elements in an array by index. Know how to iterate over elements in an array. Know of some basic functions (like finding the index of an element in an array).                                                                                                                                                                                                                                             |
| `attributes`                 | Know what attributes are. Know how to annotate code with attributes. Know how to pass properties to attributes.                                                                                                                                                                                                                                                                                                                                                                                       |
| `basics`                     | Know what a variable is. Know how to define a variable. Know how to update a variable. Know how to use type inference for variables. Know how to define a method. Know how to return a value from a method. Know how to call a method. Know that methods must be defined in classes. Know about the `public` access modifier. Know about the `static` modifier. Know how to define an integer. Know how to use mathematical operators on integers. Know how to define single- and multiline comments. |
| `bit-manipulation`           | Know how to use bitwise operators to manipulate bits.                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| `classes`                    | Know what classes are. Know what encapsulation is. Know what fields are. Know how to create an object. Know how to update state through methods. Know about the `void` type.                                                                                                                                                                                                                                                                                                                          |
| `conditionals`               | Know of the existence of the `if` conditional execution statement.                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| `custom-attributes`          | Know of the existence of the `Attribute` type. Know what attributes are for. Know how to define custom attributes. Know how to read attribute values at runtime. Know how to limit attribute usage.                                                                                                                                                                                                                                                                                                   |
| `datetimes`                  | Know of the existence of the `DateTime` type. Know how to create a `DateTime` instance. Know how to get the current date. Know of the individual, date- and time-related properties. Know how to access the current date. Know how to compare dates. Know how to convert a `string` to a `DateTime` and vice versa                                                                                                                                                                                    |
| `enums`                      | Know of the existence of the `enum` keyword. Know how to define enum members. Know how to assign values to enum members. Know how to get an enum's numeric value. Know how to convert a `string` to an `enum`.                                                                                                                                                                                                                                                                                        |
| `flag-enums`                 | Know how to define a "flags" enum. Know how to add, remove or check for flags. Know how to change the underlying type of an enum.                                                                                                                                                                                                                                                                                                                                                                     |
| `floating-point-numbers`     | Know of the existing of the three floating point types: `double`, `float` and `decimal`. Know when to use which floating point type.                                                                                                                                                                                                                                                                                                                                                                  |
| `foreach-loops`              | Know how to iterate over a collection using a `foreach` loop.                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| `pattern-matching-constants` | Know how to use the `switch` statement to do constant pattern matching.                                                                                                                                                                                                                                                                                                                                                                                                                               |
| `numbers`                    | Know of the existence of the two most commonly used number types, `int` and `double`, and understand that the former represents whole numbers, and the latter floating-point numbers. Know of basic operators such as multiplication, comparison and equality. Know how to convert from one numeric type to another. Know what implicit and explicit conversions are.                                                                                                                                 |
| `properties`                 | Know what properties are and how they relate to fields and methods. Know what backing-field properties are. Know what auto-implemented properties are. Know what calculated properties are. Know how to use property accessors to customize visibility. Know how to define the different types of properties.                                                                                                                                                                                         |
| `strings`                    | Know of the existence of the `string` type. Know how to create a string. Know of some basic methods (like finding the index of a character in a string, or returning a part the string). Know how to do basic string formatting.                                                                                                                                                                                                                                                                      |
| `while-loops`                | Know how to write a `while` loop.                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |

[anonymous_functions]: ../../../reference/concepts/anonymous_functions.md
[array]: ../../../reference/types/array.md
[bool]: ../../../reference/types/boolean.md
[char]: ../../../reference/types/char.md
[class]: ../../../reference/types/class.md
[classes]: ../../../reference/concepts/classes.md
[composition]: ../../../reference/concepts/composition.md
[conditionals]: ../../../reference/concepts/conditionals.md
[encapsulation]: ../../../reference/concepts/encapsulation.md
[enumeration]: ../../../reference/concepts/enumeration.md
[equality]: ../../../reference/concepts/sameness.md
[functions]: ../../../reference/types/function.md
[generics]: ../../../reference/concepts/generics.md
[higher_order_functions]: ../../../reference/concepts/higher_order_functions.md
[immutability]: ../../../reference/concepts/immutability.md
[inheritance]: ../../../reference/concepts/inheritance.md
[interfaces]: ../../../reference/concepts/interfaces.md
[issues-improve-reference]: https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+label%3Atrack%2Fcsharp+label%3Atype%2Fimprove-reference+label%3Astatus%2Fhelp-wanted
[issues-new-reference]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Atrack%2Fcsharp+label%3Atype%2Fnew-reference+label%3Astatus%2Fhelp-wanted+
[linq]: ../../../reference/concepts/pipelines.md
[list]: ../../../reference/types/list.md
[local_functions]: ../../../reference/concepts/nested_functions.md
[map]: ../../../reference/types/map.md
[methods]: ../../../reference/concepts/methods.md
[mutation]: ../../../reference/concepts/mutation.md
[null]: ../../../reference/types/null.md
[nullable]: ../../../reference/types/nullable.md
[objects]: ../../../reference/concepts/objects.md
[optional_arguments]: ../../../reference/concepts/default_arguments.md
[pattern_matching]: ../../../reference/concepts/pattern_matching.md
[polymorphism]: ../../../reference/concepts/polymorphism.md
[queue]: ../../../reference/types/deque.md
[range]: ../../../reference/types/range.md
[recursion]: ../../../reference/concepts/recursion.md
[set]: ../../../reference/types/set.md
[stack]: ../../../reference/types/stack.md
[state]: ../../../reference/concepts/state.md
[string]: ../../../reference/types/string.md
[struct]: ../../../reference/types/struct.md
[tuple]: ../../../reference/types/tuple.md
[type_inference]: ../../../reference/concepts/type_inference.md
[variables]: ../../../reference/concepts/variables.md
