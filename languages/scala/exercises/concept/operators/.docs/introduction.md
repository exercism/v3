## Operators

Like any other programming language Scala provides all typical operators for native types.

Examples of arithmetic operators:

```scala
5.3 + 7.1 // Double = 12.4
5 - 2 // Int = 3
4L * 8L // Long = 32L
14 / 4 // Int = 3
14 % 4 // Int = 2
14.0 / 4.0 // Double = 3.5
```

Examples of relational and logical operators:

```scala
4 > 6 // Boolean = false
4 == 6 // Boolean = false
4 != 6 // Boolean = true
'A' <= 'C' // Boolean = true
4 != 6 || 4 == 6 // Boolean = true
4 != 6 && 4 == 6 // Boolean = false
!(4 > 6) // Boolean = true
```

In Scala operators like `+` are just ordinary methods. If you write `1 + 2`, it is equal to the expression `1.+(2)`, which means, `Int` has a method named `+`, which takes one argument of type `Int`.

Try out by yourself and evaluate both expressions.

Using `+` in the "operator style":

```scala
1 + 2
```

Using `+` in the "method style":

```scala
1.+(2)
```

In this exercise we will define `+` and `*` operators for an own complex type.
