Swift has values of a number of fundamental data types that are similar to what is seen in other programming languages, including integers, booleans, and floating point numbers. In addition to these types, Swift also offers a number of more complex types that values can be, including arrays, dictionaries, and functions.

Values can be associated with [names][naming] in one of two ways, by assigning them to a variable or by assigning them to a constant. That name may then be used to refer to that value throughout the program. Constants are immutable, which means that the value cannot be changed. Variables, on the other hand, are mutable, which means that the value can be changed at any time.

Once you’ve declared a constant or variable of a certain type, you can’t declare it again with the same name, or change it to store values of a different type. Nor can you change a constant into a variable or a variable into a constant.

Variables are defined using the `var` keyword, while constants are defined using the `let` keyword.

```swift
var variableName = 10 // variable
let constantName = 10 // constant
```

Swift is a type-safe, statically typed language, which means that the types of all values are known at compile time and that one type cannot be used in your program where a different type is expected. This will prevent you from, e.g. passing it an Int to a function that expects a floating point value. Type safety helps you catch and fix type errors as early as possible in the development process.

In addition, Swift also offers [type inference][type inference], which means that the compiler will attempt to determine the type of a value or expression from the program itself, rather than needing to be told explicitly what the type should be.

```swift
// Automatically inferred type
let applesInBasket = 10
```

However, if needed for clarity or to help the compiler with its inference, one may also use [type annotations][type annotations] to explicitly specify the type of a value.

```swift
var myFloatingPointVar: Double = 10 // Explicitly make this a floating point value rather than an integer
```

[Functions][functions] are also values that are associated with names, though the syntax used to declare a function differs from that used to declare a constant or a variable. They are defined using the `func` keyword followed by a pair of parentheses enclosing a comma-separated list of parameter names where these names include an [argument label and a parameter name][argument labels] followed by a [type annotation][type annotations]. The argument label is used to refer to a parameter when the function is _called_ and the parameter name is used to refer to the parameter's value inside the function body. The parameter list is followed by `->` and the type of the value returned by the function. Note that the type annotations for the parameters are required and can not be inferred by the compiler. If the return type of the function is `Void`, i.e. `()`, the `-> ()` or `-> Void` may be omitted.

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

Swift supports two types of [comments][comments]. Single line comments are preceded by `//` and multiline comments are inserted between `/*` and `*/`.

[naming]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID313
[type inference]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID322
[type annotations]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID312
[functions]: https://docs.swift.org/swift-book/LanguageGuide/Functions.html
[argument labels]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID166
[comments]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID315
