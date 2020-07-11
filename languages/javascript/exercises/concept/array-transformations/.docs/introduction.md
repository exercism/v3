In Javascript, the Array class has many powerful built-in functions for transforming arrays.  These functions make it much easier to do things than it otherwise might be using a simple for loop.

Example of transformation using a for loop :

```javascript
const numbers = [1,2,3]
...
// => [6,7,8]
```

Example of transformation using a built-in method:

```javascript
const numbers = [1,2,3]
numbers.map((n) => n + 5)
// => [6,7,8]
```