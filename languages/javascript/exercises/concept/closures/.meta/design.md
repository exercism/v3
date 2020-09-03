This issue describes how to implement the `closures` concept exercise for the `javascript` track.

## Goal

The goal of this exercise is to teach the student how _closures_ are implemented in `JavaScript`.

In other words: how _values_ can be _enclosed_ in a _function_. It touches onto `scoping` and `functions` in general, and is usually used as `higher-order-functions`, `callbacks` or to change `visibility` (think: private).

## JavaScript specificness

ℹ You might be inclined to think that this is equivalent to **anonymous functions**, but that is not the case. In most languages I know supporting closures, these are modelled as anonymous functions (functions without a name), such as inline blocks, proc, lambda or even function definitions. The JavaScript equivalent is:

```javascript
// As function argument
someFunctionCall(() => { /* my anonymous arrow function declaration */ })

// As return value
function anotherFunctionCall() {
  return function () {
    /* anonymous function declaration */
  }
)
```

However, in JavaScript, the function name can be derived from the _original binding_. This is **not common\*. What does this mean? The following examples are **not\*\* anonymous functions:

```javascript
const adder = (a, b) => a + b

adder.name
// => "adder"

let value = 0
var increment = function () {
  value += 1
  return value
}

increment.name
// "increment"

const rebound = increment
rebound.name
// "increment"
```

So in JavaScript, the anonymous-ness doesn't determine if you can _enclose_ something. In fact, you can _always_ enclose values, regardless if you're using a:

- function declaration
- anonymous function declaration
- function expression
- anonymous function expression
- `Function` object/instance/class with evaluation\*
- `const`, `let` or `var`

```javascript
let a = 'hello world'
const print = new Function('console.log(a)')

print.name
// => "anonymous"

print()
// => prints "hello world"
```

## Learning objectives

- Function that returns an "enclosed" value (think `private`)
- Function that _mutates_ an "enclosed" value (think `increment`/`id` generation)
- Function that combines an "enclosed" value with some input (think higher order/adder)
- Variable scope
- Creating closures in loops (A common mistake)

## Out of scope

- Shadowing values (unless you can make this very concise)
- Hoisting (especially: enclosing `var`)
- Context (`this`, `arguments`, `new.target`)

## Concepts

- `closures`

## Prerequisites

- `basics`
- `booleans`
- `arrays`
