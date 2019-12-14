# Dart

## Primitives

- [string][type-string]
- [int][type-number]
- [boolean][type-boolean]
- [null][type-null]
- [symbol][type-symbol]

All primitives are [immutable][concept-immutable], i.e., they cannot be altered. It is important not to confuse a primitive itself with a variable assigned a primitive value. The variable may be reassigned a new value, but the existing value can not be changed in the ways that [objects][type-object], iterables, and [functions][type-function] can be altered.

## Non-primitive types

- [`object`][type-object].

## Boxed types / Wrappers

Except for `null` and `undefined`, all primitive values have [`object`][type-object] equivalents that wrap around the primitive values:

- `String` for the [`string`][type-string] primitive
- `Number` for the [`number`][type-number] primitive
- `Boolean` for the [`bool`][type-boolean] primitive
- `Symbol` for the [`symbol`][type-symbol] primitive.

The wrapper's `valueOf()` method returns the primitive value.

Note: Wrapped primitives (using the `new Constructor()` syntax are of `typeof` [`object`][type-object], and not their primitive type).

[type-boolean]: ../../types/boolean.md
[type-function]: ../../types/function.md
[type-null]: ../../types/null.md
[type-number]: ../../types/number.md
[type-object]: ../../types/object.md
[type-string]: ../../types/string.md
[type-symbol]: ../../types/symbol.md
[type-undefined]: ../../concepts/undefined.md

## Global Objects (not "types")

- [`Boolean`][global-object-boolean]
- [`Date`][global-object-date]
- [`Error`][global-object-error]
- [`Function`][global-object-function]
- `Iterable`
- [`JSON`][global-object-json]
- `Number`
- [`Object`][global-object-object]
- [`RegExp`][global-object-regexp]
- `String`
- [`Map`][global-object-map]
- [`Set`][global-object-set]

[global-object-error]: ./types/error.md
[global-object-function]: ./types/function.md
[global-object-json]: ./types/json.md
[global-object-map]: ./types/map.md
[global-object-object]: ./types/object.md
[global-object-regexp]: ./types/regexp.md
[global-object-set]: ./types/set.md

## Expressions

- [`new`][keyword-new]
- `this`

## Object-oriented concepts

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

## Functional concepts

- [Anonymous functions][concept-anonymous-functions]
- [Higher-order functions][concept-higher-order-functions]
- [Immutability][concept-immutable] (of primitives, and using `Object.freeze`)
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

## General concepts

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

## Language specific concepts

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

## Other important/interesting things

- ECMAScript standard
- Engines
- Web APIs
- Web workers are not the same as multi-thread programming

### user-land concepts

- Stream based programming (subscriptions)
- Declarative programming (Flutter)
- Compilers (AOT, JIT)
- Bundlers (Pub)
