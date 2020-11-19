Swift is a type-safe, statically typed language, which means that the types of all values are known at compile time and that one type cannot be used in your program where a different type is expected. This will prevent you from, e.g. passing an `Int` to a function that expects a floating point value. Type safety helps you catch and fix type errors as early as possible in the development process.

In addition, Swift also offers [type inference][type inference], which means that the compiler will attempt to determine the type of a value or expression from the program itself, rather than needing to be told explicitly what the type should be.

```swift
// Automatically inferred type
let applesInBasket = 10
```

However, if needed for clarity or to help the compiler with its inference, one may also use [type annotations][type annotations] to explicitly specify the type of a value.

```swift
// Explicitly make this a floating point value rather than an integer
var myFloatingPointVar: Double = 10
```

[type inference]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID322
[type annotations]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID312
