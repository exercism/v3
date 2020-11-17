## function-overloading
## multiple-return-values
## default-parameters
## variadic-parameters
## inout-parameters
## nested-functions

We have seen in the `basics` exercise how functions in Swift are defined. Here we will look at some additional features of functions in Swift.

## Multiple return values

Multiple values can be returned from Swift functions by creating and returning a tuple from the different values.

```swift
func reverseAndLength(_ str: String) -> (reverse: String, length: Int) {
  return (reverse: str.reverse, length: str.count)
}

reverseAndLength("Hello")
// => (reverse: "olleH", length: 5)
```

## Default parameter values

Default parameter values can be supplied for any of a function's parameters by assigning a value to the parameter in the parameter list following the parameter's type annotation. When a default parameter value is specified, the caller of the function can omit that parameter when calling the function and the default value will be used instead.

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

## Variadic parameters

_Variadic parameters_ in Swift allow zero or more values of the same type to be passed into a single parameter in a function. This is indicated by appending `...` to the type annotation of the parameter.

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

## In-out parameters

Within the body of a function, parameters are treated as constants, not variables; trying to modify the value of a parameter will raise a compile-time error. If a function wishes to modify the value of a parameter, it must use an _in-out parameter_ to make this mutability explicit.

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

## Nested functions

Functions may be defined inside of other functions. This is commonly used to create helper functions which are only useful to their enclosing function and so don't need to pollute the outside namespace.

These functions are defined and called just like normal functions, but are not visible outside the enclosing function.

```swift
func makeNumber(_ bits: [Bool]) -> Int {
  func double(_ x: Int) -> Int { 2 * x }
  func add(_ x: Int) -> Int { x + 1 }

  var number = 0
  for bit in bits {
    number = double(number)
    if bit {
      number = add(number)
    }
  }
  return number
}

makeNumber([true, false, true, true])
// => 11
makeNumber([true, true, false, false, false, true, true])
// => 99
```
