[Arrays][array] are one of Swift's three primary collection types. Arrays are ordered lists of elements where the elements can be of any type, however, all elements of any given list must have the same type.

Arrays literals are written as a series of elements, separated by commas, enclosed in square brackets. Empty arrays are just a pair of square brackets. Type names for arrays are written in one of two ways: `Array<T>` or `[T]` where `T` is the type of the elements in thee array. When creating an empty array, the type must be specified.

```swift
let evenInts: Array<Int> = [2, 4, 6, 8, 10, 12]
var oddInts: [Int] = [1, 3, 5, 7, 9, 11, 13]
let greetings = ["Hello!", "Hi!", "¡Hola!"]
var myStringArray: [String] = []
```

Elements of an array can be accessed individually by supplying the index of the element inside square brackets following the array; array indices are `Int`s and start with `0` for the first (leftmost) element. This subscript notation can be used to get the element at that index as well as to set the element at that index, provided the array was defined as a variable (i.e. using `var`).

Trying to access elements at indicies outside the valid range of indices will result in a runtime error that crashes the program.

```swift
evenInts[2]
// returns 6
oddInts[0] = 27
// oddInts is now [27, 3, 5, 7, 9, 11, 13]

// these operations are not allowed
greetings[3]
// error: Execution was interrupted, reason: EXC_BAD_INSTRUCTION (code=EXC_I386_INVOP, subcode=0x0).
evenInts[1] = 0
// Cannot assign through subscript: 'evenInts' is a 'let' constant
```

[array]: https://developer.apple.com/documentation/swift/array
