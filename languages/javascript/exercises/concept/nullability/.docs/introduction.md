## Nullability

The `null` value in Javascript represents the intentional absence of an object value. It is one of Javascript's primitive
types.

```javascript
// I do not have an apple.
var apple = null;
apple; // => null

// null is treated as falsy for boolean operations, therefore
!apple; // => true
!!apple; // => false
```

The `null` value has to be differentiated from the variable `undefined`. The distinction being that `undefined` is the
value of an uninitialized variable or type, while `null` represents a missing object.

```javascript
typeof null // => "object"
typeof undefined // => "undefined"
null === undefined // => evaluates to false by identity comparison
null == undefined // => evaluates to true by truthy comparison
```
