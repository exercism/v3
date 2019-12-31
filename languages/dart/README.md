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

[type-boolean]: ../../types/boolean.md
[type-function]: ../../types/function.md
[type-number]: ../../types/number.md
[type-object]: ../../types/object.md
[type-string]: ../../types/string.md
[type-symbol]: ../../types/symbol.md

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

[concept-arithmetic]: ../../concepts/arithmetic.md
[concept-bitwise-manipulation]: ../../concepts/bitwise_manipulation.md
[concept-boolean-logic]: ../../concepts/boolean_logic.md
[concept-comments]: ../../concepts/comments.md
[concept-conditionals]: ../../concepts/conditionals.md
[concept-destructuring]: ../../concepts/destructuring.md
[concept-destructuring-assignment]: ../../concepts/destructuring_assignment.md
[concept-duck-typing]: ../../concepts/duck_typing.md
[concept-enumeration]: ../../concepts/enumeration.md
[concept-functions]: ../../concepts/functions.md
[concept-loops]: ../../concepts/loops.md
[concept-methods]: ../../concepts/methods.md
[concept-rest-parameters]: ../../concepts/rest_parameters.md
[concept-scope]: ../../concepts/scope.md
[concept-type-casting]: ../../concepts/type_casting.md
[concept-variables]: ../../concepts/variables.md

### Object-oriented

- [Classes][concept-classes]
- [Composition][concept-composition]
- [Encapsulation][concept-encapsulation] (see also [anonymous functions][concept-anonymous-functions] and [scope][concept-scope])
- Inheritance
- [Mutation][concept-mutation]
- [Objects][concept-objects]
- [Polymorphism][concept-polymorphism] (see also [duck-typing][concept-duck-typing])
- [State][concept-state]

[concept-classes]: ../../concepts/classes.md
[concept-composition]: ../../concepts/composition.md
[concept-encapsulation]: ../../concepts/encapsulation.md
[concept-inheritance]: ../../concepts/inheritance.md
[concept-mutation]: ../../concepts/mutation.md
[concept-objects]: ../../concepts/objects.md
[concept-polymorphism]: ../../concepts/polymorphism.md
[concept-state]: ../../concepts/state.md

### Functional

- [Anonymous functions][concept-anonymous-functions]
- [Higher-order functions][concept-higher-order-functions]
- [Immutability][concept-immutable]
- [Nested functions][concept-nested-functions]
- [Partial application][concept-partial-application]
- [Pure functions][concept-pure-functions]
- [Recursion][concept-recursion]
- [Type inference][concept-type-inference]

[concept-anonymous-functions]: ../../concepts/anonymous_functions.md
[concept-higher-order-functions]: ../../concepts/higher_order_functions.md
[concept-immutable]: ../../concepts/immutability.md
[concept-nested-functions]: ../../concepts/nested_functions.md
[concept-partial-application]: ../../concepts/partial_application.md
[concept-pure-functions]: ../../concepts/pure_functions.md
[concept-recursion]: ../../concepts/recursion.md
[concept-type-inference]: ../../concepts/type_inference.md

### Language specific concepts

- [`async`][keyword-async]/[`await`][keyword-await] [concurrency][concept-concurrency] model
- Arguments
  - All arguments are optional
  - Named arguments via [destructuring][concept-destructuring]
- [Generators][concept-generators]
- Package Management

[concept-concurrency]: ../../concepts/concurrency.md
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
