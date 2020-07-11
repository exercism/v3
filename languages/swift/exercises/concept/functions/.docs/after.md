We have seen in the `basics` exercise how [functions][functions] in Swift are defined using the `func` keyword followed by parentheses enclosed list of parameter names where these names include an [optional argument label and a parameter name][argument labels] followed by a [type annotation][type annotations] and the body of the function enclosed in curly braces. E.g.

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

## Multiple return values

[Multiple values can be returned][multiple-return-values] from Swift functions by creating and returning a tuple from the different values.

```swift
func reverseAndLength(_ str: String) -> (reverse: String, length: Int) {
  return (reverse: str.reverse, length: str.count)
}

reverseAndLength("Hello")
// => (reverse: "olleH", length: 5)
```

## Implicit returns

In cases where the entire body of a function is a single expression, [the `return` keyword may be omitted][implicit-returns]. So the previous example can also be written as:

```swift
func reverseAndLength(_ str: String) -> (reverse: String, length: Int) {
  (reverse: str.reverse, length: str.count)
}

reverseAndLength("Hello")
// => (reverse: "olleH", length: 5)
```

## Default parameter values

[Default parameter values][default-parameter-values] can be supplied for any of a function's parameters by assigning a value to the parameter in the parameter list following the parameter's type annotation. When a default parameter value is specified, the caller of the function can omit that parameter when calling the function and the default value will be used instead.

```swift
func greeting(name: String = "guest", duration: Int = 2) -> String {
  "Welcome, \(name). Enjoy your \(duration) night stay."
}

greeting(name: "Bobo", duration: 7)
// => "Welcome, Bobo. Enjoy your 7 night stay."
greeting(duration: 3)
// => "Welcome, guest. Enjoy your 3 night stay."
greeting(name: "Wynona")
// => "Welcome, Wynona. Enjoy your 2 night stay."
greeting()
// => "Welcome, guest. Enjoy your 2 night stay."
```

Swift style suggests, but does not require, that default parameters be placed at the end of the parameter list, as they are commonly viewed as less important parameters for the function.

## Variadic parameters

[_Variadic parameters_][variadic-parameters] in Swift allow zero or more values of the same type to be passed into a single parameter in a function. This is indicated by appending `...` to the type annotation of the parameter.

These values will be automatically grouped into an array with elements of the same type as the type of the variadic parameter.

```swift
func geometricMean(_ numbers: Double...) -> Double {
  var total = 1.0
  for number in numbers {
      total *= number
  }
  return pow(total, 1.0 / Double(numbers.count))
}

geometricMean(1, 2, 3, 4, 5)
// => 2.605171084697352
```

Swift only allows functions to have one variadic parameter. Additionally, if the function has parameters that follow the variadic parameter in the definition, the first parameter following the variadic parameter is _required_ to have an argument label.

```swift
func geometricMean(_ numbers: Double..., greeting: String, _ otherGreeting: String) -> Double {
    var total: Double = 1
    print(greeting, otherGreeting)
    for number in numbers {
        total *= number
    }
  return pow(total, 1.0 / Double(numbers.count))
}

geometricMean(1, 2, 3, 4, 5, greeting: "Yo!", "Whaddup?")

// => prints Yo! Whaddup?, returns 2.605171084697352
```

## In-out parameters

Within the body of a function, parameters are treated as constants, not variables; trying to modify the value of a parameter will raise a compile-time error. If a function wishes to modify the value of a parameter, it must use an [_in-out parameter_][in-out-parameters] to make this mutability explicit.

To use in-out parameters, a programmer must be sure of three things:

1. The function definition must explicitly mark in-out parameters by writing the keyword `inout` between the `:` and the type name in the parameter's type annotation.
2. The value passed in to the in-out parameter must be declared as a variable, not a constant. Similarly it cannot be a literal.
3. The variable name must be preceded by an `&` in the call to the function, explicitly marking it as modifiable.

When this is done, the function will make an internal copy of the in-out parameter, modify it as directed in the body of the function and then copy the modified value back into the variable that was passed in when the function returns.

```swift
func updateVersion(_ record: inout (version: Int, title: String)) {
  record.version += 1
}

var dbRecord = (version: 2, title: "Exercism")
updateVersion(&dbRecord)

// dbRecord is now (3, "Exercism")
```

There are a couple of extra rules one should be aware of regarding in-out parameters.

1. Inside a function with in-out parameters, you are not allowed to reference the variable that was passed in as the in-out parameter.
2. The same variable cannot be passed as multiple in-out parameters in the same function.

```swift
func inoutFunc(_ ioVar1: inout Int, _ ioVar2: inout Int) {
  ioVar1 += 1
  ioVar2 += 2
}

var mutVar = 0
inoutFunc(&mutVar, &mutVar)
// raises a compiler error: "Inout arguments are not allowed to alias each other"
```

## Nested functions

Functions may be defined [inside of other functions][nested-functions]. This is commonly used to create helper functions which are only useful to their enclosing function and so don't need to pollute the outside namespace.

These functions are defined and called just like normal functions, but are not visible outside the enclosing function.

```swift
func makePangram(_ str: String) -> String {
  func missingLetters() -> String {
    let base = str.lowercased() + "isay"
    var missing = ""
    for char in "abcdefghijklmnopqrstuvwxyz" {
      if char.isLetter && !base.contains(char) {
        missing.append(char)
      }
    }
    print("missing:", missing)
    return missing
  }
  return "\(str) I say, \"\(missingLetters())\""
}

makePangram("The quick brown fox jumped over the lazy dog.")
// => "The quick brown fox jumped over the lazy dog. I say, """
makePangram("Zelda quits. Just as you were awakening.")
// => "Zelda quits. Just as you were awakening. I say, "bcfhmpvx""
```

[functions]: https://docs.swift.org/swift-book/LanguageGuide/Functions.html
[type annotations]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID312
[argument labels]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID166
[multiple-return-values]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID164
[implicit-returns]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID607
[default-parameter-values]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID169
[variadic-parameters]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID171
[in-out-parameters]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID173
[nested-functions]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID178
