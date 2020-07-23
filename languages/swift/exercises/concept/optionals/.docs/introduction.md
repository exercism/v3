## Optionals

Swift uses _optionals_ to allow programmers to represent the possible absence of a value. Before attempting to use a value that may not exist, optionals allow the program to check first it it exists, then if it does exist unwrap and use it.

Any type can be made into an optional by appending a `?` onto the end of the type name. So an optional integer would have type `Int?` and an optional string would have type `String?`. Defining constants or variables of optional type and assigning them values is done the same as for values of non-optional types.

```swift
let x: Int? = 42
var y: String? = "Hello"
y = "Goodbye"
```

You can assign the absence of a value to a variable of optional type by assigning it the special value `nil`. `nil` can be used with all optional types, but `nil`s assigned to two different optional types do not have the same type, and cannot be interchanged or even compared. E.g.

```swift
let intOpt: Int? = nil
let stringOpt: String? = nil

intOpt = stringOpt
// Compiler error: Cannot assign value of type 'String?' to type 'Int?'

intOpt == stringOpt
// Compiler error: Binary operator '==' cannot be applied to operands of type 'Int?' and 'String?'
```

An example of where optionals arise in Swift is in the initialization of `Int`s and `Double`s from strings. For example, you can convert the string `"123"` to an integer by writing `let newInt = Int("123")`. However, if you do this you will find that the type of `newInt` is not `Int`, but rather `Int?`. This is because not all strings can sensibly be converted to `Int`s. What should the result of `Int("123.45")` or `Int("horse")` be. In cases like this, where there is no sensible value to return, the conversion returns `nil`, and so the return type must be `Int?`.

## Using optionals

Because optional types are not the same types as their base types, the two types cannot be used in the same ways. For example:
`Int("123") + 1` results in a compiler error "Value of optional type 'Int?' must be unwrapped to a value of type 'Int'". In order to access the `Int` from the conversion, one must "unwrap" it first.

This is most commonly done in Swift using the `if-let` and `guard-let` constructs for _optional binding_ which check for `nil` and take one code path with the unwrapped value bound to a supplied name if a value exists and taking a different code path if `nil` was found.

```swift
if let num = Int("123") {
	let sum = num + 1
	…
} else {
  // code for the case where nil was found -- may be left out
}
```

The `guard-let` option may also be used in the cases where early return is desired:

```swift
guard let num = Int("123") else { return nil }
let sum = num + 1
…
```

## Comparing optionals

Note that even if the base type of a pair of optionals can be compared using the standard comparison operators, the optionals themselves cannot be compared. They can only be checked for equality. two optionals are equal if they are both nil or if the values they wrap are equal within their base types.

However, code can of course, be written to perform a custom comparison of two optional values. Below is an example of a `switch` statement that will return `true` only if both optional values are non-nil and the first value is less than the second. To do this it uses the _optional pattern_ `varName?` which only matches non-nil optionals, binding the value inside the optional to the name `varName`:

```swift
switch (optionalA, optionalB) {
case let (valA?, valB?): return valA < valB
default: return false
}
```
