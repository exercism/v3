# Logical NOT (`!`)

The following code shows examples of the ! (logical NOT) operator.

```javascript
n1 = !true // !t returns false
n2 = !false // !f returns true
n3 = !'' // !f returns true
n4 = !'Cat' // !t returns false
```

## Double NOT (`!!`)

It is possible to use a couple of NOT operators in series to explicitly force the conversion of any value to the corresponding [boolean primitive][type-boolean]. The conversion is based on the "truthyness" or "falsyness" of the value (see [truthy and falsy][concept-truthy_and_falsy].

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Logical_Operators#Logical_NOT

[concept-truthy_and_falsy]: ../../../reference/concepts/truthy_and_falsy.md
[type-boolean]: ../../../reference/types/boolean.md
