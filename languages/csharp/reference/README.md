# C&#35; reference

## Concepts

The C# concept exercises are based on concepts. The list below contains the concepts that have been identified for the C# language.

### Object-oriented

The core OO features a C# developer should know about are:

- [Classes][classes] ✅ (exercise [`classes`][exercise-classes])
- [Composition][composition]
- [Encapsulation][encapsulation] ✅ (exercise [`classes`][exercise-classes])
- Fields ✅ (exercise [`classes`][exercise-classes])
- Indexers
- [Inheritance][inheritance] ✅ (exercise [`inheritance`][exercise-inheritance])
- [Interfaces][interfaces] 🚧 (issue [#1225](https://github.com/exercism/v3/issues/880))
  - Explicit Interface Implementation 🚧 (issue [#1225](https://github.com/exercism/v3/issues/880))
  - Marker Interfaces
- [Methods][methods] ✅ (exercise [`basics`][exercise-basics])
  - Constructors ✅ (exercise [`constructors`][exercise-constructors])
  - Destructors
  - Extension methods 📖 (issue [#1070](https://github.com/exercism/v3/issues/1070))
  - Method arguments 📖 (issue [#1128](https://github.com/exercism/v3/issues/1128))
    - Named arguments 📖 (issue [#1128](https://github.com/exercism/v3/issues/1128))
    - Out parameters 📖 (issue [#1128](https://github.com/exercism/v3/issues/1128))
    - [Optional arguments][optional_arguments] 📖 (issue [#1128](https://github.com/exercism/v3/issues/1128))
  - Method overloading ✅ (exercise [`method-overloading`][exercise-method-overloading])
  - Return values ✅ (exercise [`basics`][exercise-basics])
- [Mutation][mutation] ✅ (exercise [`basics`][exercise-basics])
- [Objects][objects] ✅ (exercise [`classes`][exercise-classes])
  - Object initializers
- [Polymorphism][polymorphism] ✅ (exercise [`inheritance`][exercise-inheritance])
- Properties ✅ (exercise [`properties`][exercise-properties])
- [State][state] ✅ (exercise [`classes`][exercise-classes])
- Statics
- Structs 📖 (issue [#1017](https://github.com/exercism/v3/issues/1017))

### Functional

While C# is primarily an OO language, lots of functional concepts have been added to the language:

- Expressions vs statements
  - Expression-bodied members 📖 (issue [#1145](https://github.com/exercism/v3/issues/1145))
- [Functions][functions] ✅ (exercise [`basics`][exercise-basics])
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
  - Pattern matching constants ✅ (exercise [`enums`][exercise-enums])
  - Pattern matching types 📖 (issue [#546](https://github.com/exercism/v3/issues/546))
  - Pattern matching tuples 📖 (issue [#964](https://github.com/exercism/v3/issues/964))
- [Recursion][recursion]
- [Type inference][type_inference] ✅ (exercise [`basics`][exercise-basics])

### General

- Asynchronous programming
- Attributes 📖 (issue [#1176](https://github.com/exercism/v3/issues/1176))
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
- Comments ✅ (exercise [`basics`][exercise-basics])
- Comparison
  - [Equality][equality] (`Equals`, `GetHashCode`) 📖 (issue [#1016](https://github.com/exercism/v3/issues/1016))
  - Ordering
- Concurrency
  - Concurrent collections
  - Locks
- Conditionals
  - Boolean logic ✅ (exercise [`booleans`][exercise-booleans])
  - Conditionals: do-while
  - Conditionals: switch
  - Conditionals: while ✅ (exercise [`floating-point-numbers`][exercise-floating-point-numbers])
  - [Conditionals: if][conditionals] ✅ (exercise [`conditionals`][exercise-conditionals])
  - [Conditionals: ternary][conditionals]
- Constants/readonly 📖 (issue [#1044](https://github.com/exercism/v3/issues/1044))
- Conversions
  - Boxing/unboxing
  - Explicit (casts) 📖 (issue [#1142](https://github.com/exercism/v3/issues/1142))
  - Implicit 📖 (issue [#1142](https://github.com/exercism/v3/issues/1142))
- Enumeration
  - [Enumeration: for loop][enumeration]
  - [Enumeration: foreach loop][enumeration]
- Exceptions 📖 (issue [#966](https://github.com/exercism/v3/issues/966))
  - User-defined exceptions 📖 (issue [#1141](https://github.com/exercism/v3/issues/1141))
- Null ✅ (exercise [`nullability`][exercise-nullability])
  - Null-coalescing operator ✅ (exercise [`nullability`][exercise-nullability])
  - Null-conditional operator ✅ (exercise [`nullability`][exercise-nullability])
  - Null-forgiving operator ✅ (exercise [`nullability`][exercise-nullability])
  - Nullable values ✅ (exercise [`nullability`][exercise-nullability])
- Numbers
  - Arithmetic overflow 📖 (issue [#1138](https://github.com/exercism/v3/issues/1138))
  - Bitwise manipulation ✅ (exercise [`flag-enums`][exercise-flag-enums])
  - Math operators ✅ (exercise [`numbers`][exercise-numbers])
- Randomness
- Reflection
- Regular expressions
- Resources
  - Resource cleanup (`IDisposable`)
  - Resource lifetime
  - Resource passing (by reference/by value)
  - [Resource allocation][memory_allocation] 📖 (issue [#1018](https://github.com/exercism/v3/issues/1018))
  - Resource pooling 📖 (issue [#1146](https://github.com/exercism/v3/issues/1146))
- Scoping
  - Imports (usings)
  - Namespaces 📖 (issue [#1127](https://github.com/exercism/v3/issues/1127))
  - Visibility (`public`, `private`, etc.) ✅ (exercise [`classes`][exercise-classes])
- Serialization
- Slicing
- String formatting 📖 (issue [#962](https://github.com/exercism/v3/issues/962))
  - Formatting types 📖 (issue [#962](https://github.com/exercism/v3/issues/962))
  - Interpolation 📖 (issue [#962](https://github.com/exercism/v3/issues/962))
  - StringBuilder 📖 (issue [#962](https://github.com/exercism/v3/issues/962))
- Unsafe code
- [Variables][variables] ✅ (exercise [`basics`][exercise-basics])
  - Assignment ✅ (exercise [`basics`][exercise-basics])
  - Default values (a `bool` being `false` by default, etc.)

### Types

- Anonymous types
- [Booleans][bool] ✅ (exercise [`booleans`][exercise-booleans])
- [Characters][char] 📖 (issue [#960](https://github.com/exercism/v3/issues/960))
- Collections
  - [Arrays][array] (exercise [`arrays`][exercise-arrays])
    - multi-dimension (incl. Array.CreateInstance)
  - [Dictionaries][map] ✅ (exercise [`dictionaries`][exercise-dictionaries])
  - Enumerables
  - Immutable collections
  - [Lists][list] 📖 (issue [#958](https://github.com/exercism/v3/issues/958))
  - [Queues][queue]
  - [Ranges][range]
  - [Sets][set]
  - [Stacks][stack]
- Dates ✅ (exercise [`datetimes`][exercise-datetimes])
  - Time zones
- Delegates
- dynamic
- Enums ✅ (exercise [`enums`][exercise-enums])
  - Flag enums ✅ (exercise [`flag-enums`][exercise-flag-enums])
- Events
- Indexes
- Lazy&lt;T&gt;
- Nested types
- Numbers ✅ (exercise [`numbers`][exercise-numbers])
  - Floating point numbers ✅ (exercise [`floating-point-numbers`][exercise-floating-point-numbers])
  - Signed integers 📖 (issue [#780](https://github.com/exercism/v3/issues/780))
  - Unsigned integers 📖 (issue [#780](https://github.com/exercism/v3/issues/780))
- Pointers 📖 (issue [#1147](https://github.com/exercism/v3/issues/1147))
- Streams
- [Strings][string] ✅ (exercise [`strings`][exercise-strings])
- Tasks
- Time
- [Tuples][tuple] 📖 (issue [#963](https://github.com/exercism/v3/issues/963))

## Concept interpretation

The concept exercises use the following concepts:

| concept                      | interpretation                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| ---------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `arrays`                     | Know of the existence of the `Array` type. Know how to define an array. Know how to access elements in an array by index. Know how to update an element in an array by index. Know how to iterate over elements in an array. Know of some basic functions (like finding the index of an element in an array).                                                                                                                                                                                         |
| `attributes`                 | Know what attributes are. Know how to annotate code with attributes. Know how to pass properties to attributes.                                                                                                                                                                                                                                                                                                                                                                                       |
| `basics`                     | Know what a variable is. Know how to define a variable. Know how to update a variable. Know how to use type inference for variables. Know how to define a method. Know how to return a value from a method. Know how to call a method. Know that methods must be defined in classes. Know about the `public` access modifier. Know about the `static` modifier. Know how to define an integer. Know how to use mathematical operators on integers. Know how to define single- and multiline comments. |
| `constructors`               | Know what constructors are. Know how to define parameterless constructors. Know how to define parameterized constructors. Know how to use constructor overloading. Know how to define private constructors.                                                                                                                                                                                                                                                                                           |
| `bit-manipulation`           | Know how to use bitwise operators to manipulate bits.                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| `booleans`                   | Know of the existence of the `bool` type and its two values. Know about boolean operators and how to build logical expressions with them. Know of the boolean operator precedence rules.                                                                                                                                                                                                                                                                                                              |
| `classes`                    | Know what classes are. Know what encapsulation is. Know what fields are. Know how to create an object. Know how to update state through methods. Know about the `void` type.                                                                                                                                                                                                                                                                                                                          |
| `conditionals`               | Know of the existence of the `if` conditional execution statement.                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| `custom-attributes`          | Know of the existence of the `Attribute` type. Know what attributes are for. Know how to define custom attributes. Know how to read attribute values at runtime. Know how to limit attribute usage.                                                                                                                                                                                                                                                                                                   |
| `datetimes`                  | Know of the existence of the `DateTime` type. Know how to create a `DateTime` instance. Know how to get the current date. Know of the individual, date- and time-related properties. Know how to access the current date. Know how to compare dates. Know how to convert a `string` to a `DateTime` and vice versa                                                                                                                                                                                    |
| `dictionaries`               | Know of the existence of the `Dictionary<TKey, TValue>` type. Know how to create an instance. Know how to add and remove items, look up values, check for existence of keys and enumerate contents.                                                                                                                                                                                                                                                                                                   |
| `enums`                      | Know of the existence of the `enum` keyword. Know how to define enum members. Know how to assign values to enum members. Know how to get an enum's numeric value. Know how to convert a `string` to an `enum`.                                                                                                                                                                                                                                                                                        |
| `flag-enums`                 | Know how to define a "flags" enum. Know how to add, remove or check for flags. Know how to change the underlying type of an enum.                                                                                                                                                                                                                                                                                                                                                                     |
| `floating-point-numbers`     | Know of the existing of the three floating point types: `double`, `float` and `decimal`. Know when to use which floating point type.                                                                                                                                                                                                                                                                                                                                                                  |
| `for-loops`                  | Know how to use a `for` loop to do iteration.                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| `foreach-loops`              | Know how to iterate over a collection using a `foreach` loop.                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| `indexers`                   | Know how to implement and use an indexer property.                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| `inheritance`                | Know what inheritance is. Know how to inherit from a class. Know that all types inherit from `object`. Know what abstract and sealed classes are. Know what abstract and virtual methods are. Know how to override methods. Know about the `protected` visibility modifier.                                                                                                                                                                                                                           |
| `pattern-matching-constants` | Know how to use the `switch` statement to do constant pattern matching.                                                                                                                                                                                                                                                                                                                                                                                                                               |
| `method-overloading`         | Know what method overloading is. Know how to define overloaded methods. Know the limitations of method overloading.                                                                                                                                                                                                                                                                                                                                                                                   |
| `named-arguments`            | Know how to use named arguments.                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| `nullability`                | Know of the existence of the `null` literal. Know what a `NullReferenceException` is and when it is thrown. Know how to compare a value to `null`. Know the difference between value and reference types regarding nullability, especially pre C# 8.0. Know how to define nullable reference and value types. Know about the null-related operators (`!`, `?`, `??`). Know about basic null checking by the compiler.                                                                                 |
| `numbers`                    | Know of the existence of the two most commonly used number types, `int` and `double`, and understand that the former represents whole numbers, and the latter floating-point numbers. Know of basic operators such as multiplication, comparison and equality. Know how to convert from one numeric type to another. Know what implicit and explicit conversions are.                                                                                                                                 |
| `optional-parameters`        | Know how to define optional parameters.                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
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
[memory_allocation]: ../docs/memory_allocation.md
[exercise-arrays]: ../exercises/concept/arrays/.meta/design.md
[exercise-basics]: ../exercises/concept/basics/.meta/design.md
[exercise-booleans]: ../exercises/concept/booleans/.meta/design.md
[exercise-classes]: ../exercises/concept/classes/.meta/design.md
[exercise-constructors]: ../exercises/concept/constructors/.meta/design.md
[exercise-flag-enums]: ../exercises/concept/flag-enums/.meta/design.md
[exercise-datetimes]: ../exercises/concept/datetimes/.meta/design.md
[exercise-dictionaries]: ../exercises/concept/dictionaries/.meta/design.md
[exercise-enums]: ../exercises/concept/enums/.meta/design.md
[exercise-floating-point-numbers]: ../exercises/concept/floating-point-numbers/.meta/design.md
[exercise-inheritance]: ../exercises/concept/inheritance/.meta/design.md
[exercise-method-overloading]: ../exercises/concept/method-overloading/.meta/design.md
[exercise-nullability]: ../exercises/concept/nullability/.meta/design.md
[exercise-numbers]: ../exercises/concept/numbers/.meta/design.md
[exercise-properties]: ../exercises/concept/properties/.meta/design.md
[exercise-strings]: ../exercises/concept/strings/.meta/design.md
