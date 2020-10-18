JavaScript's array destructuring syntax is a concise way to unpack array values into distinct variables.

In this example, we assign each value in the `numberOfMoons` array to its corresponding planet:
```javascript
const numberOfMoons = [0, 2, 14]
const [venus, mars, neptune] = numberOfMoons

neptune
// => 14
```