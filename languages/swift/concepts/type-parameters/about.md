Type parameters are the analogue of function parameters when defining generic constructs. They are the names that refer to the types which are to be used in the non-generic instances of the generic just as function parameters are the names that refer to the values that are passed into the function.

In Swift, [type parameters][generic-type-parameters] are written in between matching angle brackets which are placed between the function name and its parameter list. These type parameters can then be used in place of the types in the function's signature that they are meant to replace.

```swift
func genericDoubleIfValid<T>(_ value: T, isValid: (T) -> Bool) -> (T, T)? {
  guard isValid(value) else { return nil }
  return (value, value)
}
```

Here, `T` is a stand-in for _any_ type. And is the name that is used to refer to that type through the rest of the function's definition. This allows `genericDoubleIfValid` to be called with any type, so long as that type is consistent wherever there is a `T` in the generic signature.

Type parameters may refer to any concrete type, including function types:

```swift
let funcs = genericDoubleIfValid((+), isValid: { f in f(1.0, 2.0) == 3.0 })
// => ((Double, Double) -> Double, (Double, Double) -> Double)

funcs!.0(10.0, 20.5)
// => 30.5
funcs!.1(9.99, -10.0)
// -0.01
```

Here, we pass in the `+` function for `value`. Now, there are a few different `+` functions in Swift. There is one for each of the integer types, each of the floating point types, one for strings, one for arrays, and so on. So at this point, we still do not know the type that `T` represents. However, `isValid` must have type `(T) -> Bool`. And in the function passed in as `isValid`, we see that `T` is the type of a function that takes two `Doubles` as input and returns a `Double` as output. Because of this, we know that our return value must be an optional pair of copies of the `+` function for `Doubles`, and we verify that by applying them to a couple pairs of `Doubles`.

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

### Naming type parameters

When naming type parameters one should usually choose a name that the relationship between the type parameter and its use. For example, the `convert` function above is more clearly written as:

```swift
func convert<Input, Output>(_ value: Input, isValid: (Input) -> Bool, change: (Input) -> Output) -> Output {
  …
}
```

However, in some cases there is no strong relationship and single letters, usually starting with `T` (for type) are used.

```swift
func identity<T>(_ value: T) -> T {
  return value
}
```

In either case, Apple's Swift style guidelines state:

> Always give type parameters upper camel case names (such as T and MyTypeParameter) to indicate that they’re a placeholder for a type, not a value.

[generic-type-parameters]: https://docs.swift.org/swift-book/LanguageGuide/Generics.html#ID182
