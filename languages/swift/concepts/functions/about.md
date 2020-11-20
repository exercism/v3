In Swift, [functions][functions] are also values that are associated with names, though the syntax used to declare a function differs from that used to declare a constant or a variable. They are defined using the `func` keyword followed by a pair of parentheses enclosing a comma-separated list of parameter names where these names include an [argument label and a parameter name][argument labels] followed by a [type annotation][type annotations]. The argument label is used to refer to a parameter when the function is _called_ and the parameter name is used to refer to the parameter's value inside the function body. The parameter list is followed by `->` and the type of the value returned by the function. Note that the type annotations for the parameters are required and can not be inferred by the compiler. If the return type of the function is `Void`, i.e. `()`, the `-> ()` or `-> Void` may be omitted.

let fifteen = add10(to: 5)
Finally, this is followed by the body of the function enclosed in a pair of curly braces. For example:

```swift
func add10(to x: Int) -> Int {
  return x + 10
}

// calling add10
let fifteen = add10(to: 5)
```

There are two variations of this syntax:

1. If the author of the function does not want the caller of the function to use an argument label, the label is replaced with an underscore, `_`.
2. If the author of the function wants the argument label to be the same as the parameter name, the argument label may be completely omitted.

```swift
func square(_ x: Int) -> Int {
  return x * x
}

func area(radius: Double) -> Double {
  return Double.pi * radius * radius
}

// calling square
let twentyFive = square(5)
// calling area
let circleArea = area(radius: 2.71828)
```

All three of these variants may be used within the same function definition:

```swift
func add(_ x: Int, and y: Int, doubleResult: Bool) -> Int {
  let sum = x + y
  if doubleResult {
    return sum * 2
  } else {
    return sum
  }

let twentySix = add(6, and: 7, doubleResult: true)
}
```

Within the body of a function, parameters are treated as constants, not variables.

[functions]: https://docs.swift.org/swift-book/LanguageGuide/Functions.html
[argument labels]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID166
[type annotations]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID312
