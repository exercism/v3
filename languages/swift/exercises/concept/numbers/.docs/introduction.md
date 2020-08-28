## Numbers

Swift contains many basic numeric types that can represent sets of either integer or floating-point values, where different types may be chosen depending on the size of value you require and the architecture of the computer where the application is running (e.g. 32-bit or 64-bit).

For this exercise you will only be dealing with the two most commonly used numeric types in Swift:

- `Int`: This type is used to represent signed whole numbers e.g. `0`, `255`, `2147483647`. A signed integer is at least 32 bits in size (value range of: -2147483648 through 2147483647). But this will depend on the systems architecture. Most modern computers are 64 bit, therefore `Int` will be 64 bits in size (value range of: -9223372036854775808 through 9223372036854775807).

- `Double`: This type is a 64 bit floating-point type, used to represent numbers which may have a fractional component, e.g. `0.0`, `3.14`, and `-1.36969e-10`.

Swift supports the standard set of arithmetic operators of `+`, `-`, `*`, `/` and `%` (remainder not modulo). Note that for `Int` values, `/` is the integer division operator that throws away any remainder.

When a number is written as a whole number in Swift code, without any additional context to steer the type inference engine in the right direction, the Swift compiler will assume that number is an `Int`. If you want to tell the compiler that you want it to be a `Double` you must use either a type annotation or append a .0 onto the end of the number literal. E.g.

```swift
let x = 42         // x is an Int
let y = 42.0       // y is a Double
let z: Double = 42 // z is a Double
```

## Type Conversion

In Swift, assignment of a value between different types requires explicit conversion. For example, to convert an `Int` to a `Double` and vice versa, you would need to do the following:

````swift
let x = 42
let d = Double(x)

let pi = Double.pi
let iPi = Int(pi)

print("x:", x, "is of type:", type(of: x))
// Output: x: 42 is of type: Int

print("d:", d, "is of type:", type(of: d))
// Output: d: 42.0 is of type: Double

print("pi:", pi, "is of type:", type(of: pi))
// Output: pi: 3.141592653589793 is of type: Double

print("iPi:", iPi, "is of type:", type(of: iPi))
// Output: iPi: 3 is of type: Int```
````
