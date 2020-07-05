## General

- There are three [boolean operators][logical operators] to work with boolean values.
- Multiple operators can be combined in a single expression.

## 1. Check if a fast attack can be made

- The logical NOT operator (`!`) can be placed before an expression to negate its value.

## 2. Check if a spy action can be made

- Logical operators apply to boolean values and yield a result of the same type as the operands. The right operand is evaluated conditionally.


## 3. Check if a signal action can be made

- Logical operators execute in the order of their precedence (from highest to lowest): `!`, `&&`, `||`.
- For more details check out the Operator Precendence section on the [oficial golang documentation][operators] and the [truth table][truth table].

[logical operators]: https://golang.org/ref/spec#Logical_operators
[operators]: https://golang.org/ref/spec#Operators
[truth table]: https://www.digitalocean.com/community/tutorials/understanding-boolean-logic-in-go


