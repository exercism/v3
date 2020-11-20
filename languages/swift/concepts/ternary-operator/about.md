Swift also supports a fourth type of conditional, [the ternary operator][ternary-operator]. The ternary operator is an operator that takes, as the name implies, three inputs. The first input is a Boolean expression, and the other two inputs are expressions of the same type. The structure of the ternary operator is:

```swift
Boolean-expression ? expression1 : expression2
```

The ternary operator evaluates _Boolean-expression_ then, if the expression evaluates to `true`, evaluates _expression1_, returning its value, otherwise it evaluates _expression2_ and returns its value. For example, the following expression assigns the larger of the two values `a` and `b` to `bigger`.

```swift
let bigger = a > b ? a : b
```

[ternary-operator]: https://docs.swift.org/swift-book/LanguageGuide/BasicOperators.html#ID71
