# JavaScript

## Primitives

- [string][type-string]
- [number][type-number]
- [bigint][type-bigint]
- [boolean][type-boolean]
- [null][type-null]
- [undefined][type-undefined]
- [symbol][type-symbol]

All primitives are [immutable][concept-immutable], i.e., they cannot be altered. It is important not to confuse a primitive itself with a variable assigned a primitive value. The variable may be reassigned a new value, but the existing value can not be changed in the ways that [objects][type-object], [arrays][type-array], and [functions][type-function] can be altered.

## Non-primitive types

- [`object`][type-object]

Note: Things like `Array` and `Date` are not types, but rather constructors that, when used, create `object`s with a certain [_prototype_][concept-prototype-inheritance].

## Boxed types / Wrappers

Except for `null` and `undefined`, all primitive values have [`object`][type-object] equivalents that wrap around the primitive values:

- [`String`][global-object-string] for the [`string`][type-string] primitive
- [`Number`][global-object-number] for the [`number`][type-number] primitive
- [`BigInt`][global-object-bigint] for the [`bigint`][type-bigint] primitive
- [`Boolean`][global-object-boolean] for the [`boolean`][type-boolean] primitive
- [`Symbol`][global-object-symbol] for the [`symbol`][type-symbol] primitive.

The wrapper's `valueOf()` method returns the primitive value.

Note: Wrapped primitives (using the `new Constructor()` syntax are of [`typeof`][keyword-typeof] [`object`][type-object], and not their primitive type). See [prototype-based inheritance][concept-prototype-inheritance].

[type-array]: ../../reference/types/array.md
[type-bigint]: ../../reference/types/big_integer.md
[type-boolean]: ../../reference/types/boolean.md
[type-function]: ../../reference/types/function.md
[type-null]: ../../reference/types/null.md
[type-number]: ../../reference/types/number.md
[type-object]: ../../reference/types/object.md
[type-string]: ../../reference/types/string.md
[type-symbol]: ../../reference/types/symbol.md
[type-undefined]: ../../reference/concepts/undefined.md

## Global Objects (not "types")

- [`Array`][global-object-array]
- [`Boolean`][global-object-boolean]
- [`Date`][global-object-date]
- [`Error`][global-object-error]
- [`Function`][global-object-function]
- [`JSON`][global-object-json]
- [`Math`][global-object-math]
- [`Number`][global-object-number]
- [`Object`][global-object-object]
- [`RegExp`][global-object-regexp]
- [`String`][global-object-string]
- [`Map`][global-object-map]
- [`Set`][global-object-set]
- [`WeakMap`][global-object-weakmap]
- [`WeakSet`][global-object-weakset]
- ...and [others][concept-global-objects]

[global-object-array]: ./objects/array.md
[global-object-boolean]: ./objects/boolean.md
[global-object-bigint]: ./objects/bigint.md
[global-object-date]: ./objects/date.md
[global-object-error]: ./objects/error.md
[global-object-function]: ./objects/function.md
[global-object-json]: ./objects/json.md
[global-object-map]: ./objects/map.md
[global-object-math]: ./objects/math.md
[global-object-number]: ./objects/number.md
[global-object-object]: ./objects/object.md
[global-object-promise]: ./objects/promise.md
[global-object-regexp]: ./objects/regexp.md
[global-object-set]: ./objects/set.md
[global-object-string]: ./objects/string.md
[global-object-symbol]: ./objects/symbol.md
[global-object-typed-array]: ./objects/typed-array.md
[global-object-weakmap]: ./objects/weakmap.md
[global-object-weakset]: ./objects/weakset.md

## Expressions

- [`instanceof`][keyword-instanceof] (using [prototype][concept-prototype-inheritance])
- [`typeof`][keyword-typeof] (using [type][concept-prototype-inheritance])
- [`new`][keyword-new]
- `this`

## Object-oriented concepts

- [Classes][concept-classes]
- [Composition][concept-composition]
- [Encapsulation][concept-encapsulation] (see also [anonymous functions][concept-anonymous-functions] and [scope][concept-scope])
- [Inheritance][concept-inheritance] (see also [prototype-based inheritance][concept-prototype-inheritance])
- [Mutation][concept-mutation]
- [Objects][concept-objects] (see also [global objects][concept-global-objects])
- [Polymorphism][concept-polymorphism] (see also [duck-typing][concept-duck-typing])
- [State][concept-state]

[concept-classes]: ../../reference/concepts/classes.md
[concept-composition]: ../../reference/concepts/composition.md
[concept-encapsulation]: ../../reference/concepts/encapsulation.md
[concept-inheritance]: ../../reference/concepts/inheritance.md
[concept-mutation]: ../../reference/concepts/mutation.md
[concept-objects]: ../../reference/concepts/objects.md
[concept-polymorphism]: ../../reference/concepts/polymorphism.md
[concept-state]: ../../reference/concepts/state.md

## Functional concepts

- [Anonymous functions][concept-anonymous-functions]
- [Higher-order functions][concept-higher-order-functions]
- [Immutability][concept-immutable] (of primitives, and using `Object.freeze`)
- [Nested functions][concept-nested-functions]
- [Partial application][concept-partial-application]
- [Pure functions][concept-pure-functions]
- [Recursion][concept-recursion]
- [Type inference][concept-type-inference]

[concept-anonymous-functions]: ../../reference/concepts/anonymous_functions.md
[concept-higher-order-functions]: ../../reference/concepts/higher_order_functions.md
[concept-immutable]: ../../reference/concepts/immutability.md
[concept-nested-functions]: ../../reference/concepts/nested_functions.md
[concept-partial-application]: ../../reference/concepts/partial_application.md
[concept-pure-functions]: ../../reference/concepts/pure_functions.md
[concept-recursion]: ../../reference/concepts/recursion.md
[concept-type-inference]: ../../reference/concepts/type_inference.md

## General concepts

- [Arithmetic][concept-arithmetic]
- [Bitwise manipulation][concept-bitwise-manipulation]
- [Boolean logic][concept-boolean-logic] (see also [jsdoc][platforms-jsdoc])
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

[concept-arithmetic]: ../../reference/concepts/arithmetic.md
[concept-bitwise-manipulation]: ../../reference/concepts/bitwise_manipulation.md
[concept-boolean-logic]: ../../reference/concepts/boolean_logic.md
[concept-comments]: ../../reference/concepts/comments.md
[concept-conditionals]: ../../reference/concepts/conditionals.md
[concept-destructuring]: ../../reference/concepts/destructuring.md
[concept-destructuring-assignment]: ../../reference/concepts/destructuring_assignment.md
[concept-duck-typing]: ../../reference/concepts/duck_typing.md
[concept-enumeration]: ../../reference/concepts/enumeration.md
[concept-functions]: ../../reference/concepts/functions.md
[concept-loops]: ../../reference/concepts/loops.md
[concept-methods]: ../../reference/concepts/methods.md
[concept-rest-parameters]: ../../reference/concepts/rest_parameters.md
[concept-scope]: ../../reference/concepts/scope.md
[concept-type-casting]: ../../reference/concepts/type_casting.md
[concept-variables]: ../../reference/concepts/variables.md

## Language specific concepts

- [`async`][keyword-async]/[`await`][keyword-await] [concurrency][concept-concurrency] model
- Arguments
  - All arguments are optional
  - Named arguments via [destructuring][concept-destructuring]
- [Event loop][concept-event-loop] (see also: [Events][concept-events])
- [Generators][concept-generators]
- [Hoisting][concept-hoisting]
- [Iterators][concept-iterators]
- [Modules][concept-modules] ([`import`][keyword-import], [`require`][keyword-require])
- Package Management
- [Prototype based inheritance][concept-prototype-inheritance]
- [Sameness][concept-sameness]
- [Strict mode][concept-strict-mode]
- [Typed arrays][global-object-typed-array]
- Types vs. objects (see also [prototype-based inheritance][concept-prototype-inheritance])
  - [Global objects][concept-global-objects]
  - [Arrays are objects, with indexer][global-object-array]
  - [Functions are callable objects][global-object-function]

[concept-concurrency]: ../../reference/concepts/concurrency.md
[concept-events]: ../../languages/javascript/info/events.md
[concept-event-loop]: ../../languages/javascript/info/event_loop.md
[concept-generators]: ../../languages/javascript/info/generators.md
[concept-global-objects]: ../../languages/javascript/info/global_objects.md
[concept-hoisting]: ../../languages/javascript/info/hoisting.md
[concept-iterators]: ../../languages/javascript/info/iterators.md
[concept-modules]: ../../languages/javascript/info/modules.md
[concept-prototype-inheritance]: ../../languages/javascript/info/prototype_inheritance.md
[concept-sameness]: ../../languages/javascript/info/sameness.md
[concept-strict-mode]: ../../languages/javascript/info/strict_mode.md
[keyword-async]: ./keywords/async.md
[keyword-await]: ./keywords/await.md
[keyword-new]: ./keywords/new.md
[keyword-import]: ./keywords/import.md
[keyword-instanceof]: ./keywords/instanceof.md
[keyword-require]: ./keywords/require.md
[keyword-typeof]: ./keywords/typeof.md
[platforms-jsdoc]: ../../reference/tooling/jsdoc.md

## Other important/interesting things

- ECMAScript standard
- Engines
- Web APIs
- Node APIs
- web workers are not the same as multi-thread programming

### user-land concepts

- Stream based programming (rxjs, observable -> might make it into language)
- Declarative programming (Vue, React etc)
- Transpilers (Babel, Buble, TypeScript, etc) -> corejs compatibility etc.
- Bundlers (Parcel, Webpack, Rollup, etc)
