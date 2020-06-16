# Scala reference

## Concepts

The Scala concept exercises are based on concepts. The list below contains the concepts that have been identified for the Scala language.

### Language-unique

- Implicits
- Path-dependent types

### Functional

- [Functions][functions]
  - [Pure functions][pure_functions]
  - [Higher-order functions][higher_order_functions]
  - [Anonymous functions][anonymous_functions]
  - [Nested functions][nested_functions]
  - [Function composition][function_composition]
  - [Partial application][partial_application]
  - [Recursion][recursion]
    - Tail recursion
  - [Partial function][partial_function]
  - Currying
  - Implicit returns
  - Type annotations
- [Immutability][immutability]
- [Pattern matching][pattern_matching]
  - Discards
  - Guard clauses
- [Type inference][type_inference]
  - Automatic generalization
- [Pipelines][pipelines]
- [Expression-oriented (not statement oriented)][expression_oriented]
- Lazy value

### Object-oriented

The core OO features an Scala developer should know about are:

- [Classes][classes]
  - [case class][case class]
- [Encapsulation][encapsulation]
- [Objects][objects]
- [Interfaces][interfaces]
  - Object expressions
  - Traits
  - self-types
- [Methods][methods]
  - Constructors
  - Apply
  - Unapply
  - Method arguments
    - Named arguments
    - [Optional arguments][optional_arguments]
    - Out parameters
    - implicit arguments
- Fields
- Properties
- Type extensions
- Implicit class
  - enrich my library

While Scala has OO features, it is a "functional first" language. It is important to know how and when to apply the OO features, and which OO features to use.

### General

- Values
  - Assignment
- Scoping
  - Namespaces
  - Modules
    - Extending modules
  - Imports (`import`)
  - Visibility (`public`, `private`, etc.)
- Numbers
  - Math operators
  - Bitwise manipulation
  - Arithmetic overflow
- Comparison
  - [Equality][equality] (`equals`, `hashCode`)
  - Ordering
- Conditionals
  - Boolean logic
  - [Conditionals: if][conditionals]
  - Conditionals: while
- Enumeration
  - [Enumeration: for loop][enumeration]
- Collections
  - [Generics][generics]
  - Sequence/for expressions
  - Iterators (yield)
  - Collections: mapping
  - Collections: filtering
  - Collections: reducing
  - Collections: ordering
  - Collections: combining
  - Ranges
    - Slicing
  - mutable
  - immutable
- Resources
  - Resource passing by reference
  - Resource passing by value
  - Resource passing by name
- Conversions
  - Implicit
  - Explicit
- Concurrency
  - Concurrent collections `par`
  - Future
- Exceptions
  - `Try`
  - `Either`
- Code quotations
  - "
  - '
  - """
- quasiquotes
- Attributes
- Nullability
  - `Option`
- String formatting
  - Formatting types

### Types

- [Strings][string]
- [Characters][char]
- [Booleans][bool]
- [Option][option]
- [Either][either]
- [Try][try]
- [Future][future]
- [Tuples][tuple]
- Any
- Nothing
- Unit
- Numbers
  - Integers
  - Long
  - [Floating point numbers][floating-point-number]
  - BigInteger
  - BigDecimal
- Collections
  - [Arrays][array]
  - [Lists][list]
  - [Maps][map]
  - [Sets][set]
  - [Ranges][range]

## Concept interpretation

The concept exercises use the following concepts:

| concept | interpretation |
| ------- | -------------- |


[encapsulation]: ../../../reference/concepts/encapsulation.md
[classes]: ../../../reference/concepts/classes.md
[objects]: ../../../reference/concepts/objects.md
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
[nested_functions]: ../../../reference/concepts/nested_functions.md
[equality]: ../../../reference/concepts/sameness.md
[conditionals]: ../../../reference/concepts/conditionals.md
[enumeration]: ../../../reference/concepts/enumeration.md
[generics]: ../../../reference/concepts/generics.md
[bool]: ../../../reference/types/boolean.md
[string]: ../../../reference/types/string.md
[char]: ../../../reference/types/char.md
[array]: ../../../reference/types/array.md
[list]: ../../../reference/types/list.md
[map]: ../../../reference/types/map.md
[set]: ../../../reference/types/set.md
[class]: ../../../reference/types/class.md
[tuple]: ../../../reference/types/tuple.md
[range]: ../../../reference/types/range.md
[optional_arguments]: ../../../reference/concepts/default_arguments.md
[functions]: ../../../reference/types/function.md
[variables]: ../../../reference/concepts/variables.md
[pure_functions]: ../../../reference/concepts/pure_functions.md
[partial_function]: ../../../reference/concepts/partial_functions.md
[function_composition]: ../../../reference/concepts/function_composition.md
[partial_application]: ../../../reference/concepts/partial_application.md
[recursion]: ../../../reference/concepts/recursion.md
[pipelines]: ../../../reference/concepts/pipelines.md
[expression_oriented]: ../../../reference/concepts/expression_oriented.md
[floating-point-number]: ../../../reference/types/floating_point_number.md
