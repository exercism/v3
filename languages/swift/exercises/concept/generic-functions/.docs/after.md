One of the most important concepts in programming is code reuse. One should be able to limit the number of times they write a piece of code in order to make the code smaller and easier to understand, while at the same time lowering the amount of time and effort needed to write a program.

We've seen one of the common ways this is accomplished in programming, functions. While functions are occasionally used just to repeat the same code over and over, more often than not, they are executed with different input values. Structs, classes, and enums also allow us to reuse code involving both methods (functions) and properties (data), reusing the code in those structs, classes, and enums with many different values.

This allows for a great deal of code reuse, however, there was still a big limitation. While we could use that code for many different values, all of those values had to be of the same _type_.

---

_Generics_ are a programming language feature that Swift offers which allows programmers to write code that can be reused not just with many different values, but also with values of many different types.

For an example of the problems generics solve, consider the function

```swift
func doubleIfValid(_ i: Int, isValid: (Int) -> Bool) -> (Int, Int)? {
  guard isValid(i) else { return nil }
  return (i, i)
}

doubleIfValid(2000) { $0.isMultiple(of: 2) }
// => (2000, 2000)
doubleIfValid(42) { $0 >= 50 }
// => nil
```

As you can see we can use this function with many different values. Not only many different `Int` values, but also many different `(Int) -> Bool` values for our `isValid` parameter. This gives us great flexibility, however, if we wanted to perform the same type of computation on a `Double` or a `String`, we'd need to write brand new functions that are exactly the same, only with different type signatures (and possibly different names):

```swift
func doubleDoubleIfValid(_ i: Double, isValid: (Double) -> Bool) -> (Double, Double)? {
  guard isValid(i) else { return nil }
  return (i, i)
}

func doubleStringIfValid(_ i: String, isValid: (String) -> Bool) -> (String, String)? {
  guard isValid(i) else { return nil }
  return (i, i)
}

doubleDoubleIfValid(0.1) { $0 < 1 && $0 > 0 }
// => (0.1, 0.1)
doubleStringIfValid("Boo!") { $0.count> 5 }
// => nil
```

This is the problem that generics solve. Just like we use parameters in functions to reuse the same code with different values, we can use _type parameters_ to create generic functions that allow us to reuse the same code with different types.

---

### Generic functions

[Generic functions][generic-functions] are written with [type parameters][generic-type-parameters] listed in between matching angle brackets which are placed between the function name and its parameter list. These type parameters can then be used in place of the types in the function's signature that they are meant to replace.

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

And this works for any type, including function types:

```swift
let funcs = genericDoubleIfValid((+), isValid: { f in f(1.0, 2.0) == 3.0 })
// => ((Double, Double) -> Double, (Double, Double) -> Double)

funcs!.0(10.0, 20.5)
// => 30.5
funcs!.1(9.99, -10.0)
// -0.01
```

Here, we pass in the `+` function for `value`. Now, there are a few different `+` functions in Swift. There is one for each of the integer types, each of the floating point types, one for strings, one for arrays, and so on. So at this point, we still do not know the type that `T` represents. However, `isValid` must have type `(T) -> Bool`. And in the function passed in as `isValid`, we see that `T` is the type of a function that takes two `Doubles` as input and returns a `Double` as output. Because of this, we know that our return value must be an optional pair of copies of the `+` function for `Doubles`, and we verify that by applying them to a couple pairs of `Doubles`.

---

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

[generic-functions]: https://docs.swift.org/swift-book/LanguageGuide/Generics.html#ID181
[generic-type-parameters]: https://docs.swift.org/swift-book/LanguageGuide/Generics.html#ID182
