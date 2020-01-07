_Note: All subconcepts should be split out into separate files under the concepts directory and hyperlinked._

# F&#35;

## Language-unique concepts

These are concepts that, as far as I know, are either unique to F# or shared with only a few languages:

- Dependency order
- Computation expressions
- Type providers
- Active patterns
- Pipeline operator
- Double-quoted identifiers (many more characters allowed in names)
- Units of measure
- Code quotations
- Reference cells
- Object expressions
- byref (dealing with memory)

## Functional concepts

Thinking as an F# developer means knowing about how to use the following functional programming concepts in F# (as F# is, after all, a functional-first language):

- [Immutability](../../reference/concepts/immutability.md)
- [Pattern matching](../../reference/concepts/pattern_matching.md)
- [Function composition](../../reference/concepts/function_composition.md)
- [Pure functions](../../reference/concepts/pure_functions.md)
- [Higher-order functions](../../reference/concepts/higher_order_functions.md)
- [Nested functions](../../reference/concepts/nested_functions.md)
- [Anonymous functions](../../reference/concepts/anonymous_functions.md)
- [Partial application](../../reference/concepts/partial_application.md)
- [Type inference](../../reference/concepts/type_inference.md)
- [Expression-oriented (not statement oriented)](../../reference/concepts/expression_oriented.md)
- [Recursion (and tail-recursion)](../../reference/concepts/recursion.md)
- [Pipelines (for, while, map, filter, reduce)](../../reference/concepts/pipelines.md)
- [REPL](../../reference/concepts/repl.md)
- Railway-oriented programming (not a language feature, but a common way of dealing with chaining error states)
- Monads
- Currying
- REPL

## Object-oriented concepts

While F# is "functional first" language, it does have (very nice) object-orientation features. It is important to know how and when to apply the OO features, and which OO features (see https://youtu.be/yL7xBhWrdKw?t=2248) to use. The core OO features an F# developer should know about are:

- [Classes](../../reference/concepts/classes.md)
- [Interfaces](../../reference/concepts/interfaces.md)
- [State](../../reference/concepts/state.md)
- [Mutation](../../reference/concepts/mutation.md)
- Dot notation (instead of using functions to access data)
- Static/non-static members
- Named arguments
- Optional arguments
- Indexers
- Inheritance

Note that some of these features are specific to object-orientation in the world of .NET.

## Platform-specific concepts

- .NET interop (how to integrate with other .NET libraries, which are mostly written in C#. good interop was key in the F# design!)
- [Assemblies](../../reference/tooling/dotnet-assemblies.md)

## General concepts

- Automatic generalisation (linked to type inference)
- Scoping through indentation (whitespace significant)
- Ranges
- Slicing (linked to ranges)
- [Generics](../../reference/concepts/generics.md)
- Modules
- Namespaces
- Attributes
- Implicit returns (last statement is returned)
- Messaging and agents
- Type abbreviations
- Exception handling
- Type casting
- Type extensions
- Signatures
- Reflection

## Types

- Record type (product type, immutable, easy to copy and modify, structural semantics)
- Discriminated union (sum type, linked to (exhaustive) pattern matching)
- Option type (no null by default)
- Result type (encode errors in type system)
- Tuple (product type, it's not a type common to all languages, so I did not list it as such)
- Unit type
- Array
- List
- Map
- [Set](../../reference/types/set.md)
- Sequence
- Anonymous records
- Events
- Structs
- Delegates
- Enums

## Resources used

- Official language reference: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/
- F# for fun and profit: https://fsharpforfunandprofit.com/
