_Generics_ are a programming language feature that Swift offers which allows programmers to write code that can be reused not just with many different values, but also with values of many different types.

One aspect of generics that Swift offers are generic functions, which allow us to write functions that can perform the same operations on values of different types, rather than just different values of the same type.

Generic functions are written with type parameters listed in between matching angle brackets which are placed between the function name and its parameter list. These type parameters can then be used in place of the types in the function's signature that they are meant to replace.

```swift
func genericDoubleIfValid<T>(_ value: T, isValid: (T) -> Bool) -> (T, T)? {
  guard isValid(value) else { return nil }
  return (value, value)
}
```

Here, `T` is a stand-in for _any_ type. This allows `genericDoubleIfValid` to be called with any type, so long as that type is consistent wherever there is a `T` in the generic signature. This means that if we choose to pass in an `Int` for `value` we must also pass in an `(Int) -> Bool` function for `isValid`, and we will then get an `(Int, Int)?` as a return value. And if we choose to pass in an `String` for `value` we must also pass in an `(String) -> Bool` function for `isValid`, and we will then get an `(String, String)?` as a return value.

These generic functions are called in the same way as regular functions.

```swift
genericDoubleIfValid(170) { $0.isMultiple(of: 2) }
// => (170, 170)
genericDoubleIfValid("Books!") { $0.count > 5 }
// => ("Books!", "Books!")
```

There can be more than one type parameters for any generic function. Like regular parameters, there can be as many as are needed to do the job. For example, if we don't necessarily want our doubling function to merely return a tuple with two copies of our input value, we can add another type parameter to represent the output type and then pass another function into our function that generates the desired output.

```swift
func convert<T, U>(_ value: T, isValid: (T) -> Bool, change: (T) -> U) -> U? {
  guard isValid(value) else { return nil }
  return change(value)
}

convert(15, isValid: { $0.isMultiple(of: 5) }, change: { ($0, $0) })
// => (15, 15)
convert("Books!", isValid: { $0.count > 5 }, change:  { $0 + $0 })
// => "Books!Books!"
convert("Books!", isValid: { $0.count > 5 }, change:  { $0.count })
// => 5
```

Here we see that the three different `change` functions have types `(Int) -> (Int, Int)`, `(String) -> String`, and `(String) -> Int` respectively, showing how we may vary both type parameters.
