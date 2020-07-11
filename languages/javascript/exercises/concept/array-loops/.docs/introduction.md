<!--
This is the markdown file with the introduction to the concept and exercise.

See https://github.com/exercism/v3/blob/master/docs/concept-exercises.md#docsintroductionmd
-->


In JavaScript, arrays are used to store multiple values in a single variable! These values can be of any type you choose, and can even be of multiple types. Arrays are also given lots of built-in methods, some of which can be used to loop over the values, giving you access to a single one on each iteration.

Each of these built-in methods have a different use case, and differing syntaxes to go along with them.

Looping over an array using its prototype is as easy as invoking `Array.prototype.forEach`. `forEach` takes in two parameters: a callback function and a lexical context. The callback is a function that is called every time the loop index is increased, and takes in three parameters: the current value, the current index, and the array as a whole. The second parameter to `forEach` is useful when using this method in another prototypal context (don't worry if you don't understand this right now, we'll provide some examples.)

Note that because `forEach` doesn't directly expose the underlying `for` loops, you cannot use the `break` statement in the callback.

*Basic example of Array.prototype.forEach*:

```js
const numbers = [1, 2, 3, 4, 6, 7, 8, 9]

numbers.forEach((currentValue, currentIndex, fullArray) => {
    if (currentIndex-- = currentValue) {
        console.log(fullArray);
    }
})
```

*Using the second parameter*:

```js
function Counter() {
  this.sum = 0
  this.count = 0
}

Counter.prototype.add = function(array) {
  array.forEach((entry) => {
    this.sum += entry
    ++this.count
  }, this)
  // The use of the second parameter gives Counter.prototype.add access to the properties of the Counter instance.
}

const obj = new Counter()

obj.add([3, 4, 5])

console.log(obj.count)
// 3 
console.log(obj.sum)
// 12
```

---

Sometimes, efficiency is more valuable than convenience (for instance, when you're designing a performance-critical algorithm). In this case, you may want to `break` out of the loop at a given point. Although using `forEach` may not allow this, using a conventional `for` loop does.

*Using a for loop to iterate over an Array*:

```js
const answersToEverything = [42, 42, 42]

for (let index = 0;  index < answersToEverything.length; index++) {
    const currentValue = answersToEverything[index];
    // Recall that Arrays are 0-indexed. As such, we can just get the current element by using index.
    console.log(currentValue);
}
```

---

However, while you might access the current value in the iteration, you may not need the current index. When this is the case, using a `for .. of` loop may be more suited for the task.

A `for .. of` loop is syntactical sugar that creates a regular `for` loop over an iterable objects. It functions in the same way that a `for` loop does, but without directly exposing the current iteration index.

*Using a `for .. of` loop using `const`*:

```js
const numbers = [ 6.0221515, 10, 23  ]

// Because we won't be modifying any of the values, using const is preferable.
for (const number of numbers) {
    console.log(number)
}

// Output:
// 6.0221515
// 10
// 23
```

*Using a `for .. of` loop using `let` or `var`*:

```js
const numbers = [ 18, 62, 82  ]

// Using a mutable keyword allows us to change values in the array.
for (let number of numbers) {
    number += 1;

    console.log(number)
}

// Output:
// 19
// 63
// 83
```