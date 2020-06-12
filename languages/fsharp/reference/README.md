# F&#35; reference

## Concepts

The F# concept exercises are based on concepts. The list below contains the concepts that have been identified for the F# language.

### Language-unique

- Active patterns
- Computation expressions
- Dependency order
- Type providers
- Units of measure

### Functional

- [Expression-oriented (not statement oriented)][expression_oriented]
- [Functions][functions]
  - [Anonymous functions][anonymous_functions]
  - Currying
  - [Function composition][function_composition]
  - [Higher-order functions][higher_order_functions]
  - Implicit returns
  - [Nested functions][nested_functions]
  - [Partial application][partial_application]
  - [Pure functions][pure_functions]
  - [Recursion][recursion]
    - Tail recursion
  - Type annotations
- [Immutability][immutability]
- [Pattern matching][pattern_matching]
  - Discards
  - Guard clauses
- [Pipelines][pipelines]
- [Type inference][type_inference]
  - Automatic generalisation

### Object-oriented

The core OO features an F# developer should know about are:

- [Classes][classes]
- [Composition][composition]
- [Encapsulation][encapsulation]
- Fields
- Indexers
- [Interfaces][interfaces]
  - Object expressions
- [Methods][methods]
  - Constructors
  - Method arguments
    - Named arguments
    - [Optional arguments][optional_arguments]
    - Out parameters
- [Mutation][mutation]
  - Reference cells
- [Objects][objects]
- Properties
- [State][state]
- Structs
- Type extensions

While F# has OO features, it is a "functional first" language. It is important to know how and when to apply the OO features, and which OO features (see https://youtu.be/yL7xBhWrdKw?t=2248) to use.

### General

- Asynchronous programming
- Attributes
- Code quotations
- Collections
  - Collections: combining
  - Collections: filtering
  - Collections: mapping
  - Collections: ordering
  - Collections: reducing
  - [Generics][generics]
  - Iterators (yield)
  - Ranges
    - Slicing
  - Sequence/for expressions
- Comparison
  - [Equality][equality] (`Equals`, `GetHashCode`)
  - Ordering
- Concurrency
  - Concurrent collections
  - Locks
  - Messaging and agents
- Conditionals
  - Boolean logic
  - [Conditionals: if][conditionals]
  - Conditionals: while
- Conversions
  - Explicit
  - Implicit
- Double backtick-quoted identifiers (many more characters allowed in names)
- Enumeration
  - [Enumeration: for loop][enumeration]
- Exceptions
- Nullability
- Numbers
  - Arithmetic overflow
  - Bitwise manipulation
  - Math operators
- Resources
  - Resource allocation
  - Resource cleanup
  - Resource lifetime
  - Resource passing (by reference/by value)
- Scoping
  - Imports (`open`)
  - Modules
    - Extending modules
  - Namespaces
  - Shadowing
  - Visibility (`public`, `private`, etc.)
  - Whitespace significant
- String formatting
  - Formatting types
- Values
  - Assignment
  - Shadowing
- Concurrency

### Types

- Anonymous records
- Anonymous types
- [Booleans][bool]
- [Characters][char]
- Collections
  - [Arrays][array]
  - [Lists][list]
  - [Maps][map]
  - [Queues][queue]
  - [Ranges][range]
  - ResizeArray
  - [Sets][set]
  - [Stacks][stack]
- Discriminated unions
- Enums
- Numbers
  - [Floating point numbers][floating-point-number]
  - Signed integers
  - Unsigned integers
- Options
- [Records][record]
- Results
- [Strings][string]
- [Tuples][tuple]
- Unit

## Concept interpretation

The concept exercises use the following concepts:

| concept                  | interpretation                                                                                                                                                                                                                                                                                                                                                                                                                |
| ------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `arrays`                 | Know of the existence of the `Array` type. Know that arrays have a fixed length. Know that arrays are mutable. Know how to define an array. Accessing elements in an array by index. Know how to update a value in an array. Know some common array functions (summing values). Know how to pattern match on arrays.                                                                                                          |
| `basics`                 | Know what a value is. Know how to define a binding. Know that bindings are immutable. Know how to define scope using significant whitespace. Know that bindings require dependency order. Know how to define a function. Know how to return a value from a function. Know how to call a function. Know what type inference is. Know how type inference works for bindings. Know how to define single- and multiline comments. |
| `booleans`               | Know of the existence of the `bool` type and its two values. Know about boolean operators and how to build logical expressions with them. Know of the boolean operator precedence rules.                                                                                                                                                                                                                                      |
| `conditionals`           | Know of the existence of the `if` conditional execution expression.                                                                                                                                                                                                                                                                                                                                                           |
| `datetimes`              | Know of the existence of the `DateTime` type. Know how to create a `DateTime` instance. Know how to get the current date. Know of the individual, date- and time-related properties. Know how to access the current date. Know how to compare dates. Know how to convert a `string` to a `DateTime` and vice versa.                                                                                                           |
| `discriminated-unions`   | Know what discriminated unions are. Know how discriminated unions are different from enums. Know how to define a discriminated union, with and without data. Know how to pattern match on discriminated unions.                                                                                                                                                                                                               |
| `floating-point-numbers` | Know of the existing of the three floating point types: `double`, `float` and `decimal`. Know when to use which floating point type.                                                                                                                                                                                                                                                                                          |
| `lists`                  | Know of the existence of the `list` type. Know how to define an empty and non-empty list. Know how to add an element to a list. Know some common list functions. Know how to pattern match on lists.                                                                                                                                                                                                                          |
| `numbers`                | Know of the existence of the two most commonly used number types, `int` and `double`, and understand that the former represents whole numbers, and the latter floating-point numbers. Know of basic operators such as multiplication, comparison and equality. know how to convert from one numeric type to another using conversion operators.                                                                               |
| `pattern-matching`       | Know what pattern matching is. Know about constant, variable and wildcard patterns. Know how to apply guards to patterns. Know about exhaustiveness checking in pattern matching.                                                                                                                                                                                                                                             |
| `records`                | Know what a record is. Know when records should be used. Know how to define records. Know that records have structural equality. Know how to deconstruct records (including as parameters). Know how to pattern match on records. Know that records are immutable. Know how to return a modified copy of a record. Know how type inference works for records.                                                                 |
| `recursion`              | Know what recursion is. Know how to define a recursive function. Know how to define a recursive type. Know how to write a tail-recursive function.                                                                                                                                                                                                                                                                            |
| `strings`                | Know of the existence of the `string` type. Know how to create a string. Know of some basic methods (like finding the index of a character in a string, or returning a part the string). Know how to do basic string formatting. Know where it's documented, or at least how to search for it.                                                                                                                                |

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
[nested_functions]: ../../../reference/concepts/nested_functions.md
[linq]: ../../../reference/concepts/pipelines.md
[equality]: ../../../reference/concepts/sameness.md
[conditionals]: ../../../reference/concepts/conditionals.md
[enumeration]: ../../../reference/concepts/enumeration.md
[generics]: ../../../reference/concepts/generics.md
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
[pure_functions]: ../../../reference/concepts/pure_functions.md
[function_composition]: ../../../reference/concepts/function_composition.md
[partial_application]: ../../../reference/concepts/partial_application.md
[recursion]: ../../../reference/concepts/recursion.md
[pipelines]: ../../../reference/concepts/pipelines.md
[expression_oriented]: ../../../reference/concepts/expression_oriented.md
[repl]: ../../../reference/concepts/repl.md
[floating-point-number]: ../../../reference/types/floating_point_number.md
[record]: ../../../reference/types/record.md
