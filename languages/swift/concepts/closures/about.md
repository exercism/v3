[Closures][closures] in Swift are self-contained blocks of code that can be passed parameters to trigger their computation and return values. Closures also capture values from their environment and use them in their computations. As they are self contained, they may be passed around in a program like other values or assigned to constants and variables.

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

### Inferring closure types

When the return type or parameter types can be [inferred from context][inferring-closure-types], they can be omitted from the closure expression. Additionally, if the parameter types can be omitted, so can the parentheses around the parameter names:

```swift
let mean: (Double, Double) -> Double = { a, b in a + b / 2.0 }

func makeAdder(base: Int) -> (Int) -> Int {
  { x in base + x }
}

["apple", "ball", "carrot"].sorted(by: { s1, s2 in s1.count < s2.count })
```

- In the first example, the type can be inferred from the annotation on the constant, `mean`.
- In the second, the compiler knows from the definition of `makeAdder` that `base` is an int and that the returned function must be of type `(Int) -> Int`. From there, it can verify that if `x` is an `Int` then the closure will have type `(Int) -> Int`, and thus we can omit they type signature in the closure as `(Int) -> Int` is the only possible valid signature the closure may have.
- The third example is similar, but the compiler needs get some of the type information from the array that the method is being used with. It knows that the `sorted(by:)` method takes as a parameter a closure with two parameters that are the same type as the elements of the collection being sorted, and which returns a `Bool`, so the compiler can infer that the return type of the closure must be `Bool`. And since the method is being used with a String array, it can determine that the types of the parameters `s1`, and `s2` must both be `String`.

### Limitations of closures

As mentioned above, functions and closures are very similar in Swift, but there are some important differences:

- Closures may not use external argument labels. There is no way to define them and trying to use a parameter's name when calling the closure results in an error:

```swift
let add5 = { x in x + 5 }
add5(x: 10)
// Error: Extraneous argument label 'x:' in call
```

- Parameters in closures can not have a default parameter value.
- Parameters in closures may be variadic parameters, but they must have a name and type annotation.

[closures]: https://docs.swift.org/swift-book/LanguageGuide/Closures.html#
[closure-expressions]: https://docs.swift.org/swift-book/LanguageGuide/Closures.html#ID95
[inferring-closure-types]: https://docs.swift.org/swift-book/LanguageGuide/Closures.html#ID98

