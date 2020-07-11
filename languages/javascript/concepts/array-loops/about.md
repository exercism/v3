In JavaScript, looping over `Array`s is extremely common, seeing as they are some of the most widely-used and efficient structures to store data.

However, there are some lesser-known methods and data structures in JavaScript that handle some of the more common use cases for array looping in a more idiomatic way, like `Set`s, `Array.prototype.some`, and `Array.prototype.every`, and some immutable state prototypal functions.

_JavaScript Sets_

Just like in mathematics, a `Set` is a collection of unique values. Typecasting to a Set from an Array is much, much easier than looping over the entire Array:

```js
const numbers = [1, 1, 2, 3, 4, 5, 5, 7]

// We need to write a function that changes the above array into an array of unique values (i.e. removes duplicates)

// This is a function that uses a for .. of loop
const makeNumbersUniqueLoop = (arr) => {
  var temp = []

  for (const v of arr) if (arr.indexOf(v) === -1 && v !== '') temp.push(v)

  return temp
}

// The equivalent function using Sets and spread syntax
const makeNumbersUniqueSet = (a) => [...new Set(a)]
```

As you can see, using different data structures can often lead to cleaner (and in most cases, faster) code.

_For .. Of and Iterators_

Although we introduced `for .. of` as a method for iterating over an `Array`, it can also be used for other classes that extend the `Iterator` class, like generator functions, `Set`s, and so on.

_Interesting Links_

- [MDN Documentation for Array and its prototype](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array)
- [Array functions cheatsheet](https://dmitripavlutin.com/operations-on-arrays-javascript/)
