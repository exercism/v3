Values in Swift can be associated with [names][naming] in one of two ways, by assigning them to a variable or by assigning them to a constant. That name may then be used to refer to that value throughout the program. Constants are immutable, which means that the value cannot be changed. Variables, on the other hand, are mutable, which means that the value can be changed at any time.

Once you’ve declared a constant or variable of a certain type, you can’t declare it again with the same name, or change it to store values of a different type. Nor can you change a constant into a variable or a variable into a constant.

Variables are defined using the `var` keyword, while constants are defined using the `let` keyword.

```swift
var variableName = 10 // variable
let constantName = 10 // constant
```

[naming]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID313
