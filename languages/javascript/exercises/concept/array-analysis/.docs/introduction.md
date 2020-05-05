In JavaScript, an array is a list-like structure with no fixed length which can hold any type of primitives or objects, or even mixed types. The array elements can be accessed by their index. Arrays are also given a bunch of built-in methods. 

In this exercise we will use built-in methods to analyse the contents of an array, instead of using a `for` loop, or `.forEach`.

Example of analysis using a for loop :

```javascript
const numbers = [1, 'two', 3, 'four']
for (var i = 0; i < numbers.length; i++) {
    if (numbers[i] === 'two'){
        return numbers.[i]
    }
}
// => 1
```

Example of analysis using a build-in method:

```javascript
const numbers = [1, 'two', 3, 'four']
numbers.indexOf('two')
// => 1
```
