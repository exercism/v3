# Introduction

### Conditionals

Conditionals in Go are similar to conditionals in other languages. The underlying type of any conditional operation is the `bool` type, which can have the value of `true` or `false`. Conditionals are often used as flow control mechanisms to check for various conditions. For checking a particular case an `if` statement can be used, which executes its code if the underlying condition is `true`. For various cases an `else if` and `else` statements can be used. However, since Go allows early returns it is common to avoid the `else` statement after a return. When there are 3 or more cases it is often better to use a `switch` statement, which allows to test for multiple conditions. It also has a `default` for managing unmanaged cases.

To learn more about this topic it is recommended to check these sources:
[Go by Example: If/Else](https://gobyexample.com/if-else) and [Go by Example: Switch](https://gobyexample.com/switch).