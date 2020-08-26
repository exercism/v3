## 1. Create LogLevel enum

- You will need to define an initializer and a method along with your enum cases.

## 2. Parse log level

- Swift has some [properties][string-docs] and methods for extracting parts of a string.
- The [`switch` statement][switch-statement] is often a natural pairing with enums.

## 3. Convert log line to short format

- Methods are defined just like functions, only inside the enum body.
- [Raw Values][enum-raw-values] can be assigned to enum cases.
- The keyword `self` refers to the enum value inside a method's body.

[string-docs]: https://developer.apple.com/documentation/swift/string/
[switch-statement]: https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html#ID129
[enum-raw-values]: https://docs.swift.org/swift-book/LanguageGuide/Enumerations.html#ID149
