## Default parameter values

[Default parameter values][default-parameter-values] can be supplied for any of a function's parameters by assigning a value to the parameter in the parameter list following the parameter's type annotation. When a default parameter value is specified, the caller of the function can omit that parameter when calling the function and the default value will be used instead.

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

[default-parameter-values]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID169
