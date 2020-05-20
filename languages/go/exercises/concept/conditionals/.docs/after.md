# After

Conditionals in Go are similar to conditionals in other languages. The underlying type of any conditional operation is the `bool` type, which can have the value of `true` or `false`. Conditionals are often used as flow control mechanisms to check for various conditions. For checking a particular case an `if` statement can be used, which executes its code if the underlying condition is `true` like this:

```go
if condition {
    // conditional code
}
```

In scenarios involving more than one case many `if` statements can be chained together using the `else if` and `else` statements. However, it is a convention to avoid `else` statements as Go promotes early returns.

Go also provides a `switch` statement for more advanced scenarios. It can be used to switch on a variable's content or as a replacement for `if ... else if` statements. There is a third application for `switch` statements which will be introduced later: the `type switch`. A switch statement can have a `default` case which is selected if no other case applies.

If there are two or more `else if` statements in a single `if`, it should be replaced by a `switch` statement. A `switch` with a single case should be replaced by an `if` statement.

```go
// switch statement on variable content
switch someVar {
case "val1":
    // conditional code
case "val2", "val3", "foo":
    // conditional code
default:
    // default code
}

// switch statement using conditions (can replace `if ... else if` statements)
switch {
case someVar == "val1":
    // conditional code
case strings.HasPrefix(someVar, "val") || strings.HasPrefix(someVar, "foo"):
    // conditional code
}
```

Coming from other languages one may be tempted to try to use one-line conditionals. Go does not support ternary operators or one-line conditionals. This is a purposeful design decision to limit the amount of possibilities, making code simpler and easier to read.

To learn more about this topic it is recommended to check these sources:
[Go by Example: If/Else](https://gobyexample.com/if-else) and [Go by Example: Switch](https://gobyexample.com/switch).
