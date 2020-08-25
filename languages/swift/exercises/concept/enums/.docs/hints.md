## 1. Create LogLevel enum

- You will need to define an initializer and a method along with your enum cases.

## 2. Parse log level

- Swift has some [properties][string-first-docs] and [methods][string-dropfirst-docs] for getting [parts of a string][string-prefix-docs].
- You can use a [`switch` statement][switch-statement] to determine the appropriate log level.

## 3. Convert log line to short format

- Methods are defined just like functions, only inside the enum body.
- [Raw Values][enum-raw-values] can simplify the implementation of this method.
- You can refer to the enum value as `self` inside the method body.

[string-first-docs]: https://developer.apple.com/documentation/swift/string/2894206-first
[string-dropfirst-docs]: https://developer.apple.com/documentation/swift/string/2893343-dropfirst
[string-prefix-docs]: https://developer.apple.com/documentation/swift/string/2894830-prefix
[switch-statement]: https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html#ID129
[enum-raw-values]: https://docs.swift.org/swift-book/LanguageGuide/Enumerations.html#ID149
