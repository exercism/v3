# `const`

The `const` keyword is used to create a read-only reference to a value.
```javascript
const NAME = "Bob";
```
The value it holds is still [mutable](https://developer.mozilla.org/en-US/docs/Glossary/Mutable), but the variable identifier cannot be reassigned:
```javascript
const NAME = "Bob";

NAME = "Steve"; // TypeError: Assignment to constant variable
```
