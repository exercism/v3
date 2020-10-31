Array [destructuring assignment][array_destructuring_docs] is a concise way of extracting values from an array. Its syntax is similar to an [array literal][array_literal_resource] expression, but on the left-hand side of the assignment instead of the right.

```javascript
const frenchNumbers = ['quatre-vingts', 'quatre-vingt-dix', 'cent']
const [french80, french90, french100] = frenchNumbers

french80
// => 'quatre-vingts'
french90
// => 'quatre-vingt-dix'
french100
// => 'cent'
```

Because variables are mapped to values in the array by position, destructuring syntax can be used to assign or re-assign multiple variables in a single expression.

```javascript
let [a, b] = ['world', 'hello']
;[b, a] = [a, b]

a
// => 'hello'
b
// => 'world'
```

This works for nested arrays too.

```javascript
let [a, b, c] = ['orange', 'purple', 'green']
;[[a, b], c] = [[c, a], b]

a
// => 'green'
b
// => 'orange'
c
// => 'purple'
```

The syntax allows skipping values when mapping, for example to ignore specific positions in the array.
In the example below, imagine we have a `getUserInfo` function that returns an array containing a user's first name, last name, and street address.

````javascript
getUserInfo()
// => ["Valerie", "Noir", "Sunny Lane 523"]

const [, , streetAddress] = getUserInfo()

streetAddress
// => "Sunny Lane 523"
The assignment is also not required to use all the values.

```javascript
const [firstName, lastName] = getUserInfo()

firstName
// => "Valerie"

lastName
// => "Noir"
````

It's even possible to extract _more_ values than the array contains; the leftover variables will be assigned `undefined`. This may be useful when the amount of values isn't known ahead of time.

```javascript
const pickAtLeastOne = ['first choice', 'second choice']
const [first, second, third, fourth] = pickAtLeastOne

first
// => "first choice"

second
// => "second choice"

third
// => undefined

fourth
// => undefined
```

The array destructuring assignment can provide _default values_ in case there is none in the source array.

```
const [first, second, third="none selected", fourth] = pickAtLeastOne

third
// => 'none selected'

fourth
// => undefined
```

Javascript has a built-in operator that makes it easier to work with indefinite numbers of elements. When `...` appears on the left-hand side of an assignment, those three dots are known as the `rest` operator. The three dots together with a variable name is called a rest element. It collects zero or more values, and stores them into a single array.

```javascript
const [a, b, ...everythingElse] = [0, 1, 1, 2, 3, 5, 8]

a
// => 0
b
// => 1
everythingElse
// => [1, 2, 3, 5, 8]
```

Note that in JavaScript, unlike some other languages, a `rest` element cannot have a trailing comma. It _must_ be the last element in a destructuring assignment. The example below throws a `SyntaxError`:

```javascript
const [...items, last] = [2, 4, 8, 16]
```

When `...` appears on the right-hand side of an assignment, it's known as the `spread` operator. It expands an array into a list of elements. Unlike the rest element, it can appear anywhere in an array literal expression, and there can be more than one.

```javascript
const oneToFive = [1, 2, 3, 4, 5]
const oneToTen = [...oneToFive, 6, 7, 8, 9, 10]

oneToTen
// => [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
const woow = ['A', ...oneToFive, 'B', 'C', 'D', 'E', ...oneToFive, 42]

woow
// =>  ["A", 1, 2, 3, 4, 5, "B", "C", "D", "E", 1, 2, 3, 4, 5, 42]
```

[array_destructuring_docs]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment
[array_literal_resource]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array#Creating_an_array
