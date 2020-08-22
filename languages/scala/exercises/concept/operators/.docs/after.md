## Key Takeaways from this Exercise

**Ready to go operators**

Scala has a variety of built-in operators for basic types.

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
4 == 4 // Boolean = false
'A' <= 'C' // Boolean = true
4 > 6 || true // Boolean = true
4 > 6 && true // Boolean = false
!(4 > 6) // Boolean = true
```

If you want to manipulate at bit level, you can use Scala's bitwise operators:

```scala
1 & 2 // Int = 0, bitwise and
1 | 2 // Int = 3, bitwise or
1 ^ 3 // Int = 2, bitwise xor
~1 // Int = -2, bitwise negate
1 << 2 // Int = 4, shift first bit left by 2 positions
```

**DIY: Create your own operators**

Methods in Scala can be used like an operator: The expression `1.+(2)` can also be written in "operator style" (also called infix notation): `1 + 2`. This works with every method, which has one argument:

- `"hello".indexOf(1)` in infix notation is `"hello" indexOf 1`
- `"hello".endsWith("lo")` in infix notation is `"hello" endsWith "lo"`

If a method has more than one argument, the brackets cannot be removed:

- `"hello".replace('e', 'o')` in infix notation is `"hello" replace('e', 'o')`

Moreover, method names can consist of any characters (more specifically unicode characters) that you like, e.g. `def`&#x1F604;.
This fact allows us to define operators by ourself for new types like `Rectangle` by creating a method like `def +(Rectangle other) = ...`.

**Equality operator**

To compare equality between two values Scala has the `==` operator. It can be applied to any basic types:

```scala
4 == 6 // Boolean = false
1 != 6 // Boolean = true
```

Moreover, this operator can be used for any complex types and even for objects of different type:

```scala
List(1, 2, 3) == List(1, 2, 3) // Boolean = true
List(1, 2, 3) == List(4, 5, 6) // Boolean = false
"Sca" + "la" == "Scala" // Boolean = true

4 == 4.0 // Boolean = true
```

Even comparisons with null are possible and safe to do:

```scala
1 == null // Boolean = false
null == 1 // Boolean = false
null == null // Boolean = true
```

You might have noticed, that the behavior of `==` is different to Java's behavior: While Java checks for reference equality, Scala behind the scene calls `.equals()` for non-null values. Therefore, in Scala the usage of `==` and `!=` is much more common.

## Further links

- [Tour of Scala - Operators] (... are methods)

[tour of scala - operators]: https://docs.scala-lang.org/tour/operators.html
