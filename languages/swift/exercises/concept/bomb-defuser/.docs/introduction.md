Closures in Swift are self-contained blocks of code that can be passed parameters to trigger their computation and return values. Closures also capture values from their environment and use them in their computations. As they are self contained, they may be passed around in a program like other values or assigned to constants and variables.

Closures may sound a lot like Swift functions; they do, and for good reason. Functions in swift are just special cases of closures which are required to have a name and are defined using a slightly different syntax.

While functions in Swift are technically closures, when people refer to "closures" in Swift, they are referring to [closure expressions][closure-expressions]. Closure expressions are written as a parameter list followed by a return type and the keyword `in` followed by the body of the closure, all contained in a pair of curly braces:

```swift
{ (a: Double, b: Double) -> Double in
  return a + b / 2.0
}
```

This defines a closure expression of type `(Double, Double) -> Double`, and it can be executed in a similar manner to a function of this type, by applying it to parameters of the appropriate type:

```swift
{ (a: Double, b: Double) -> Double in
  return a + b / 2.0
}(10.0, 15.0)
// => 12.5
```

As it's not very convenient to write out the full closure expression every time one wants to execute it, closures can be assigned to names, like any other value in Swift. They can then be called by applying the name to the parameters:

```swift
let mean = { (a: Double, b: Double) -> Double in
  a + b / 2.0
}

mean(13.0, 100.0)
// => 56.5
```

As seen above, closures, like regular functions, may omit the `return` keyword if the body of the closure is a single expression.

Most often, closures are used with higher-order functions, either passed in as parameters or returned as results:

```swift
[11, 75, 3, 99, 53].contains(where: { (x: Int) -> Bool in x > 100 })
// => false

["apple", "ball", "carrot"].sorted(by: { (s1: String, s2: String) -> Bool in s1.count < s2.count })
// => ["ball", "apple", "carrot"]

func makeAdder(base: Int) -> (Int) -> Int {
  { (x: Int) -> Int in base + x }
}
```
