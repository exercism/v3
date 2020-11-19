## Function overloading

We have seen in the Lasagna exercise how functions in Swift are defined using the `func` keyword followed by parentheses enclosed list of parameter names where these names include an optional argument label and a parameter name followed by a type annotation and the body of the function enclosed in curly braces. E.g.

```swift
func add(_ x: Int, and y: Int, doubleResult: Bool) -> Int {
  let sum = x + y
  if doubleResult {
    return sum * 2
  } else {
    return sum
  }
}

let twentySix = add(6, and: 7, doubleResult: true)
// => 26
```

Two functions are considered distinct in Swift if they have a different name, _or_ any of the following are true:

- They have differing number of parameters
- Any external argument labels are different
- The type of any parameters are different

So given the definition of `add(_:and:doubleResult)` above, each of the following functions are considered different and can be defined and used in the same program:

```swift
// Different number of parameters
func add(_ x: Int, and y: Int) -> Int {
  x + y
}

let eleven = add(5, and: 6)
// => 11

// Different parameter type
func add(_ x: Double, and y: Int, doubleResult: Bool) -> Double {
  let sum = x + Double(y)
  if doubleResult {
    return sum * 2
  } else {
    return sum
  }
}

let twentySeven = add(6.5, and: 7, doubleResult: true)
// => 27.0

// Different argument label
func add(_ x: Int, and y: Int, doubleThat: Bool) -> Int {
  let sum = x + y
  if doubleThat {
    return sum * 2
  } else {
    return sum
  }
}
let thirteen = add(6, and: 7, doubleThat: false)
// => 13
```

Functions are also considered distinct if just the return value changes, though in many cases, it is necessary to provide a hint to the compiler in the form of a type annotation so it knows which version of the function to call.

```swift
func add(_ x: Int, and y: Int, doubleResult: Bool) -> Double {
  let sum = Double(x + y)
  if doubleResult {
    return sum * 2
  } else {
    return sum
  }
}

let thirty = add(6, and: 9, doubleResult: true)
// Error: Ambiguous use of 'add(_:and:doubleResult:)'

let thirty: Double = add(6, and: 9, doubleResult: true)
// => 30
```

But just changing an internal parameter name does not distinguish two functions.

```swift
func add(_ w: Int, and y: Int, doubleResult: Bool) -> Int {
  let sum = w + y
  if doubleResult {
    return sum * 2
  } else {
    return sum
  }
}
// Error: Invalid redeclaration of 'add(_:and:doubleResult:)'
```
