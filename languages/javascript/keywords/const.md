# `const`

The `const` keyword is used to create a read-only reference to a value.
```javascript
const NAME = "Bob";
```
The value it holds is not immutable, but the variable identifier cannot be reassigned:
```javascript
const NAME = "Bob";

const NAME = "Steve"; // This will fail
```
