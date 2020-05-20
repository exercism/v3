# Hints

## General

Conditionals are used to check for certain conditions and/or criteria. The most basic way of performing a conditional operation is using a single `if` statement.

## 1. Multiple cases (else if)

For various cases an `else if` and `else` statements can be used like this:

```go
if condition {
    // conditional code
} else if other_condition {
    // conditional code
} else {
    // default code
}
```

The `else` statement is any other case not covered by previous conditions. It should be noted that since Go allows early returns it is common to avoid the `else` statement after a return.

## 2. Multiple cases (logical operators)

Another way of checking for different scenarios is by chaining logical operators within the `if` statements like this:

```go
if conditionA ||conditionB {
    // conditional code
} else if conditionC && conditionD {
    // conditional code
} else {
    // default code
}
```

## 3. Three or more cases (switch)

When there are 3 or more cases it is recommended to use a `switch` statement, which allows testing for multiple conditions. It also has a `default` for managing unmanaged cases.

```go
switch {
case aCase || bCase:
    // conditional code
case otherCase:
    // conditional code
case anotherCase:
    // conditional code
default:
    // default code
}
```

Note that within each case different logical operators can be chained together to expand the evaluation.

## 4. Switch on variable content

A switch can also be used on a single variable's content:

```go
switch someVar {
case "val1":
    // conditional code
case "val2", "val3", "foo":
    // conditional code
default:
    // default code
}
```
