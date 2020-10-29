Array destructuring assignment is a concise way of extracting values from an array. Its syntax is similar to an array literal expression, but on the left-hand side of the assignment instead of the right.

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

Because variables are mapped to values in the array by position, we can use destructuring syntax to reassign multiple variables in a single expression.

```javascript
let a = 'orange'
let b = 'purple'
let c = 'green'

;[[a, b], c] = [[c, a], b]

a
// => 'green'
b
// => 'orange'
c
// => 'purple'
```

The positional mapping also means that if we want to extract fewer values than the array contains, we may need to use placeholders to specify which values should be ignored. This is only necessary if we need to "skip" over values to get to the ones we want.
In the example below, imagine we have a `getUserInfo` function that returns an array containing a user's first name, last name, and street address.

```javascript
const [, , streetAddress] = getUserInfo() // skipping

const [firstName, lastName] = getUserInfo() // no skipping
```

We can also extract _more_ values than the array contains; the leftover variables will simply be `undefined`. This might happen if we don't know ahead of time how many values the array will contain. If we want to make sure none of our variables end up `undefined`, we can set default values for them.

```javascript
const pickAtLeastOne = ['first choice', 'second choice']
const [1, 2='none selected', 3='none selected'] = pickAtLeastOne

1
// => 'first choice'
2
// => 'second choice'
3
// => 'none selected'
```

Javascript has a built-in operator that makes it easier to work with indefinite numbers of elements. When `...` appears on the left-hand side of an assignment, it's known as the `rest` operator. It collects zero or more values into a single array.

```javascript
const [a, b, ...everythingElse] = [0, 1, 1, 2, 3, 5, 8]

a
// => 0
b
// => 1
everythingElse
// => [1, 2, 3, 5, 8]
```

Note that in Javascript, unlike some other languages, a `rest` element cannot have a trailing comma. The example below would throw a `SyntaxError`:

```javascript
const [...items, last] = [2, 4, 8, 16]
```

When `...` appears on the right-hand side of an assignment, it's known as the `spread` operator. It expands an array into a list of elements.

```javascript
const oneToFive = [1, 2, 3, 4, 5]
const oneToTen = [...oneToFive, 6, 7, 8, 9, 10]

oneToTen
// => [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```
