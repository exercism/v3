## Numbers

Swift contains many basic numeric types that can represent sets of either integer or floating-point values, where different types may be choosen depending on the size of value you require and the architecture of the computer where the application is running (e.g. 32-bit or 64-bit).

## Integer types:

Swift offers signed and unsigned integer types in 8, 16, 32, and 64 bit sizes named `IntX` or `UIntX` where _X_ is the numer of bits and a leading U specifies unsigned integers. Swift also offers the `Int` and `UInt` types which are the signed and unsigned integers with a number of bits equal to the native machine word size (64 bits on most modern computers).

The default integer type in Swift is `Int`, and it is preferred to use `Int` rather than `UInt` even if you know that your valuse will be non-negative unless you scpecifically need values larger than the maximum `Int` value (`2147483647` on 32 bit systems `9223372036854775807` on 64 bit systems. According to Apple,

> A consistent use of Int for integer values aids code interoperability, avoids the need to convert between different number types, and matches integer type inference

You can read more about integer types in [A Tour of Swift: Integers][integers].

## Floating-point

Floating-point number types are used to represent numbers which may have a fractional component, e.g. `0.0`, `3.14`, and `-1.36969e-10`. Swift offers two floating-point number types: `Double` and `Float`. `Double` is a 64-bit floating point type and `Float` is a 32-bit floating bit type. Like with `Int` for the integer types, it is preferred to use `Double` as the default floating point type. According to Apple:

> Double has a precision of at least 15 decimal digits, whereas the precision of Float can be as little as 6 decimal digits. The appropriate floating-point type to use depends on the nature and range of values you need to work with in your code. In situations where either type would be appropriate, Double is preferred.

You can read more about floating-point types in [A Tour of Swift: Floating-Point Numbers][floatingpoint].

## Arithmetic operators

Swift supports the standard set of arithmetic operators of `+`, `-`, `*`, `/` and `%` (remainder not modulo). Note that for `Int` values, `/` is the integer division operator that throws away any remainder.

You can read more about the arithmetic operators in [A Tour of Swift: Arithmetic Operators][arithmeticoperators].

## Comparison operators

Swift also supports the standard set of comparison operators that are seen in C. Swift uses `!=` to test for inequality and not `<>` as in some languages. As with other languages, care should be taken when comparing two floating point values for equality.

You can read more about the arithmetic operators in [A Tour of Swift: Comparison Operators][comparisonoperators].

## Type inference

When a number is written as a whole number in Swift code, without any additional context to steer the type inference engine in the right direction, the Swift compiler will assume that number is an `Int`. If you want to tell the compiler that you want it to be a `Double` you must use either a type annotation or append a .0 onto the end of the number literal. E.g.

```swift
let x = 42         // x is an Int
let y = 42.0       // y is a Double
let z: Double = 42 // z is a Double
```

You casn read more about this in [A Tour of Swift: Type Safety and Inference][typeinference].

## Numeric Literals

Integers in Swift may be typed out in many ways, including as decimals, hexadecimals, binary, or octal numbers. Floating-point numbers may be typeed in decimal or hexadecimal.

Numeric literals may also include the underscore character (`_`) which is used to group numbers and improve readability. E.g. rather than write `18093402034`, one can more clearly write `18_093_402_034`.

For specifics on numeric literals in Swift, read [A Tour of Swift: Numeric Literals][numericliterals].

## Type Conversion

In Swift, assignment of a value between different types requires explicit conversion. For example, to convert an `Int` to a `Double` and vice versa, you would need to do the following:

```swift
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
// Output: fiPi: 3 is of type: Int
```

Note that in some cases, where the value is out of range for the target type, attempting to convert types will lead to an error. mIf the compiler can detect the problem, the code will fail to compile, otherwise, a runtime error will occur. E.g.

```swift
let tooBigByte = Int8(300)
// Compiler error: Integer literal '300' overflows when stored into 'Int8'

let negativeUInt = UInt(-1)
// Compiler error: Negative integer '-1' overflows when stored into unsigned type 'UInt'

let big = 300
let tooBigByte2 = Int8(big)
// error: Execution was interrupted, reason: EXC_BAD_INSTRUCTION (code=EXC_I386_INVOP, subcode=0x0).

let negative = -1
let negativeUInt2 = UInt(negative)
// error: Execution was interrupted, reason: EXC_BAD_INSTRUCTION (code=EXC_I386_INVOP, subcode=0x0).
```

[integers]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID310
[floatingpoint]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID321
[int]: https://developer.apple.com/documentation/swift/int
[double]: https://developer.apple.com/documentation/swift/double
[arithmeticoperators]: https://docs.swift.org/swift-book/LanguageGuide/BasicOperators.html#ID63
[comparisonoperators]: https://docs.swift.org/swift-book/LanguageGuide/BasicOperators.html#ID70
[typeinference]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID322
[numericliterals]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID323
