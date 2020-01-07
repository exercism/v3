# Dart

## Types

### Primitives

- [int][type-number]
- double
- [string][type-string]
- [boolean][type-boolean]
- list
- set
- map
- rune
- [symbol][type-symbol]

### Non-primitives

- [`object`][type-object].

[type-boolean]: ../../../reference/types/boolean.md
[type-function]: ../../../reference/types/function.md
[type-number]: ../../../reference/types/number.md
[type-object]: ../../../reference/types/object.md
[type-string]: ../../../reference/types/string.md
[type-symbol]: ../../../reference/types/symbol.md

## Expressions

- [`new`][keyword-new]
- `this`

## Concepts

### General

- [Arithmetic][concept-arithmetic]
- [Bitwise manipulation][concept-bitwise-manipulation]
- [Boolean logic][concept-boolean-logic]
- [Comments][concept-comments]
- [Conditionals][concept-conditionals]
- [Destructuring][concept-destructuring] (see also [destructuring assignment][concept-destructuring-assignment])
- [Duck typing][concept-duck-typing]
- [Enumeration][concept-enumeration]
- [Functions][concept-functions]
- [Loops][concept-loops]
- [Methods][concept-methods]
- [Rest parameters][concept-rest-parameters]
- [Scope][concept-scope]
- [Type casting][concept-type-casting] (see also [type inference][concept-type-inference])
- [Variables][concept-variables]

[concept-arithmetic]: ../../../reference/concepts/arithmetic.md
[concept-bitwise-manipulation]: ../../../reference/concepts/bitwise_manipulation.md
[concept-boolean-logic]: ../../../reference/concepts/boolean_logic.md
[concept-comments]: ../../../reference/concepts/comments.md
[concept-conditionals]: ../../../reference/concepts/conditionals.md
[concept-destructuring]: ../../../reference/concepts/destructuring.md
[concept-destructuring-assignment]: ../../../reference/concepts/destructuring_assignment.md
[concept-duck-typing]: ../../../reference/concepts/duck_typing.md
[concept-enumeration]: ../../../reference/concepts/enumeration.md
[concept-functions]: ../../../reference/concepts/functions.md
[concept-loops]: ../../../reference/concepts/loops.md
[concept-methods]: ../../../reference/concepts/methods.md
[concept-rest-parameters]: ../../../reference/concepts/rest_parameters.md
[concept-scope]: ../../../reference/concepts/scope.md
[concept-type-casting]: ../../../reference/concepts/type_casting.md
[concept-variables]: ../../../reference/concepts/variables.md

### Object-oriented

- [Classes][concept-classes]
- [Composition][concept-composition]
- [Encapsulation][concept-encapsulation] (see also [anonymous functions][concept-anonymous-functions] and [scope][concept-scope])
- Inheritance
- [Mutation][concept-mutation]
- [Objects][concept-objects]
- [Polymorphism][concept-polymorphism] (see also [duck-typing][concept-duck-typing])
- [State][concept-state]

[concept-classes]: ../../../reference/concepts/classes.md
[concept-composition]: ../../../reference/concepts/composition.md
[concept-encapsulation]: ../../../reference/concepts/encapsulation.md
[concept-inheritance]: ../../../reference/concepts/inheritance.md
[concept-mutation]: ../../../reference/concepts/mutation.md
[concept-objects]: ../../../reference/concepts/objects.md
[concept-polymorphism]: ../../../reference/concepts/polymorphism.md
[concept-state]: ../../../reference/concepts/state.md

### Functional

- [Anonymous functions][concept-anonymous-functions]
- [Higher-order functions][concept-higher-order-functions]
- [Immutability][concept-immutable]
- [Nested functions][concept-nested-functions]
- [Partial application][concept-partial-application]
- [Pure functions][concept-pure-functions]
- [Recursion][concept-recursion]
- [Type inference][concept-type-inference]

[concept-anonymous-functions]: ../../../reference/concepts/anonymous_functions.md
[concept-higher-order-functions]: ../../../reference/concepts/higher_order_functions.md
[concept-immutable]: ../../../reference/concepts/immutability.md
[concept-nested-functions]: ../../../reference/concepts/nested_functions.md
[concept-partial-application]: ../../../reference/concepts/partial_application.md
[concept-pure-functions]: ../../../reference/concepts/pure_functions.md
[concept-recursion]: ../../../reference/concepts/recursion.md
[concept-type-inference]: ../../../reference/concepts/type_inference.md

### Language specific concepts

- [`async`][keyword-async]/[`await`][keyword-await] [concurrency][concept-concurrency] model
- Arguments
  - All arguments are optional
  - Named arguments via [destructuring][concept-destructuring]
- [Generators][concept-generators]
- Package Management

[concept-concurrency]: ../../../reference/concepts/concurrency.md
[concept-generators]: ../../languages/dart/info/generators.md
[keyword-async]: ./keywords/async.md
[keyword-await]: ./keywords/await.md
[keyword-new]: ./keywords/new.md
[keyword-import]: ./keywords/import.md

### User-land concepts

- Stream based programming (subscriptions)
- Compilers (AOT, JIT)
- Package manager (Pub)

## Other important/interesting things

- ECMAScript standard
- Engines
- Web APIs
- Web workers are not the same as multi-thread programming
