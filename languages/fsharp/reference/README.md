# F&#35; reference

## Concepts

The F# concept exercises are based on concepts. The list below contains the concepts that have been identified for the F# language.

### Language-unique

- Dependency order
- Computation expressions
- Type providers
- Active patterns
- Units of measure

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
  - Currying
  - Implicit returns
  - Type annotations
- [Immutability][immutability]
- [Pattern matching][pattern_matching]
  - Discards
  - Guard clauses
- [Type inference][type_inference]
  - Automatic generalisation
- [Pipelines][pipelines]
- [Expression-oriented (not statement oriented)][expression_oriented]

### Object-oriented

The core OO features an F# developer should know about are:

- [Classes][classes]
- Structs
- [State][state]
- [Encapsulation][encapsulation]
- [Objects][objects]
- [Mutation][mutation]
  - Reference cells
- [Composition][composition]
- [Interfaces][interfaces]
  - Object expressions
- [Methods][methods]
  - Constructors
  - Method arguments
    - Named arguments
    - [Optional arguments][optional_arguments]
    - Out parameters
- Fields
- Properties
- Indexers
- Type extensions

While F# has OO features, it is a "functional first" language. It is important to know how and when to apply the OO features, and which OO features (see https://youtu.be/yL7xBhWrdKw?t=2248) to use.

### General

- Values
  - Assignment
- Scoping
  - Whitespace significant
  - Namespaces
  - Modules
    - Extending modules
  - Imports (`open`)
  - Visibility (`public`, `private`, etc.)
  - Shadowing
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
- Resources
  - Resource passing (by reference/by value)
  - Resource allocation
  - Resource cleanup
  - Resource lifetime
- Conversions
  - Implicit
  - Explicit
- Concurrency
  - Messaging and agents
  - Concurrent collections
  - Locks
- Exceptions
- Asynchronous programming
- Double backtick-quoted identifiers (many more characters allowed in names)
- Code quotations
- Attributes
- Nullability
- String formatting
  - Formatting types

### Types

- [Strings][string]
- [Characters][char]
- [Booleans][bool]
- [Records][record]
- Discriminated unions
- Enums
- Options
- Results
- [Tuples][tuple]
- Unit
- Numbers
  - Signed integers
  - Unsigned integers
  - [Floating point numbers][floating-point-number]
- Collections
  - [Arrays][array]
  - [Lists][list]
  - [Maps][map]
  - [Sets][set]
  - [Queues][queue]
  - [Stacks][stack]
  - [Ranges][range]
  - ResizeArray
- Anonymous types
- Anonymous records

## Concept interpretation

The concept exercises use the following concepts:

| concept | interpretation |
| ------- | -------------- |


## Reference docs

Reference docs are written to help explain a particular F# concept to a student when no appropriate online document can be found. They will be used when creating exercises and as references in exercise documentation.

The following reference docs have been written:

- [Assemblies][assemblies]

The following reference docs should be written:

- Naming conventions
- Code style
- Memory allocation

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
[pure_functions]: ../../../reference/concepts/pure_functions.md
[function_composition]: ../../../reference/concepts/function_composition.md
[partial_application]: ../../../reference/concepts/partial_application.md
[recursion]: ../../../reference/concepts/recursion.md
[pipelines]: ../../../reference/concepts/pipelines.md
[expression_oriented]: ../../../reference/concepts/expression_oriented.md
[repl]: ../../../reference/concepts/repl.md
[floating-point-number]: ../../../reference/types/floating_point_number.md
[record]: ../../../reference/types/record.md
