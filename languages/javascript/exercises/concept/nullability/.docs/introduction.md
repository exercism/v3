Nullability in Javascript expresses the intentional absence of any object value. It is represented by the `null` value.

```javascript
// I do not have an apple.
var apple = null;
apple; // null

// null is treated as falsy for boolean operations, therefore
!apple; //true
```

`null` is part of Javascript's primitive values, and it has to be differentiated from the variable `undefined`. The
distinction being that `undefined` is the value of an uninitialized variable or type, while `null` represents a missing object.

```javascript
typeof null // object
typeof undefined // undefined
null === undefined // false
null == undefined // true
```
