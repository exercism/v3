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

intopt == stringOpt
// Compiler error: Binary operator '==' cannot be applied to operands of type 'Int?' and 'String?'
```

Note that when declaring a variable or constant and assigning `nil` to it, a type annotation is required. This is because without the annotation, the compiler cannot determine which optional type to infer. Also, if a variable is defined with an optional type annotation, but neither a value nor `nil` is assigned to it, the variable will be automatically populated with a nil.

```swift
var a: Int?
a ?? 0       // evaluates to 0

var b = nil  // Compiler error: 'nil' requires a contextual type
```

An example of where optionals arise in Swift is in the initialization of `Int`s from strings. For example, you can convert the string `"123"` to an integer by writing `let newInt = Int("123")`. However, if you do this you will find that the type of `newInt` is not `Int`, but rather `Int?`. This is because not all strings can sensibly be converted to `Int`s. What should the result of `Int("123.45")` or `Int("horse")` be. In cases like this, where there is no sensible value to return, the conversion returns `nil`, and so the return type must be `Int?`.

You can read more about optionas at [A Tour of Swift: Optionals][optionals].

## Using optionals

Because optional types are not the same types as their base types, the two types cannot be used in the same ways. For example:
`Int("123") + 1` results in a compiler error "Value of optional type 'Int?' must be unwrapped to a value of type 'Int'". In order to access the `Int` from the conversion, one must "unwrap" it first. This can be done with the force-unwrap operator, `!`. Appending this operator to an optional value will return the base value within. However, force-unwrapping a `nil` will result in a runtime error that will crash the program.

```swift
Int("123")! + 1     // evaluates to 124
Int("123.45")! + 1  // error: Execution was interrupted, reason: EXC_BAD_INSTRUCTION (code=EXC_I386_INVOP, subcode=0x0).
```

As force unwrapping a `nil` crashes a program the use of the force-unwrap operator is _strongly_ discouraged in Swift. One can make the use of the operator safer by explicitly checking for `nil` before unwrapping:

```swift
let num = Int("123")
let sum : Int
if num != nil {
  sum = num! + 1
}
```

While is is safer, it leads to cluttered programs, so Swift also offers the `if-let` and `guard-let` constructs for _optional binding_ which check for `nil` and take one code path with the unwrapped value bound to a supplied name if a value exists and taking a different code path if `nil` was found.

```swift
if let num = Int("123") {
	let sum = num + 1
	…
} else {
  // code for the case where nil was found -- may be left out
}
```

Note that in this form of optional binding, the unwrapped value (here, `num`) is only in scope in the `if` block of code. It is not in scope in the else block or the code outside the `if let` statement.

The `guard-let` option may also be used in the cases where early return is desired:

```swift
guard let num = Int("123") else { return nil }
let sum = num + 1
…
```

With this form of optional binding, the unwrapped value (here, `num`) is available in the remainder of the scope following the `guard let` statement.

Multiple optional value checkss may be combined into a single optional binding statement by separating the checks with commas. Checks may also make use of values bound in earlier checks from the same statement. E.g.

```swift
func numberPlusDigits(_ value: String?) -> Int {
  guard
    let v = value,
    let i = Int(v)
  else { return 0 }
  return i + v.count
}

numberPlusDigits("123")    // return0 126
numberPlusDigits("Hello")  // returns 0
numberPlusDigits(nil)      // returns 0
```

Both the `if` and the `guard` form of optional binding also support binding the unwrapped value to a variable instead of a constant through the use of `if var` and `guard var`.

## Comparing optionals

Note that even if the base type of a pair of optionals can be compared using the standard comparison operators, the optionals themselves cannot be compared. They can only be checked for equality. two optionals are equal if they are both nil or if the values they wrap are equal within their base types.

## Nil coalescing

Another option for unwrapping exists where it is possible to use a default value if a `nil` is present. This can be done by using the _nil coalescing operator_, `??`. Assuming `x` is an `Int?`, if one writes `let y = x ?? 0`, then Swift will check if x is `nil`. If it is not, then it will unwrap `x` and assign the unwrapped value to `y`, and if `x` _is_ `nil`, then it will assign 0 to `y`.

Since `x ?? y` is simply shorthand for `x != nil ? x! : y`, if `x` is not nil, then the expression `y` is not evaluated at all.

Finally, it should be noted that the nil coalescing operator is right associative, which can lead to surprising results for the unaware.

```swift
let k = 42 ?? 0 + 1    // returns 42
let j = nil ?? 0 + 1   // returns 1
```

You can read further about the nil coalescing operator in [A Tour of Swift: Nil-Coalescing Operator][nilcoalescing].

[optionals]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID330
[nilcoalescing]: https://docs.swift.org/swift-book/LanguageGuide/BasicOperators.html#ID72
