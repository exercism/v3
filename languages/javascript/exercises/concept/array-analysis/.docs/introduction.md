In Javascript, an array is a list-like structure with no fixed length which can hold any type of primitives or ojects, even mixed types. The array elements can be accesed by their index, array also provide a bunch of built-in methods. in this exercise we will tackle the build-in method to analyse the array

Example of analysis with `looping` :

```javascript
const numbers = [1, 'two', 3, 'four']
for (var i = 0; i < numbers.length; i++) {
    if (numbers[i] === 'two'){
        return numbers.[i]
    }
}
// => 1
```

Example of analysis without `looping` through it:

```javascript
const numbers = [1, 'two', 3, 'four']
numbers.indexOf('two')
// => 1
```
