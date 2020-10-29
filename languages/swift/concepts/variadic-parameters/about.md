## Variadic parameters

[_Variadic parameters_][variadic-parameters] in Swift allow zero or more values of the same type to be passed into a single parameter in a function. This is indicated by appending `...` to the type annotation of the parameter.

These values will be automatically grouped into an array with elements of the same type as the type of the variadic parameter.

```swift
func geometricMean(_ numbers: Double...) -> Double {
  var total = 1.0
  for number in numbers {
      total *= number
  }
  return pow(total, 1.0 / Double(numbers.count))
}

geometricMean(1, 2, 3, 4, 5)
// => 2.605171084697352
```

When using variadic parameters, Swift has two limitations:

1. Swift only allows functions to have one variadic parameter. It has been decided that this restriction will be removed in a future release of Swift, but it is currently still in place.
2. If a function has parameters that follow the variadic parameter in the definition, the first parameter following the variadic parameter is _required_ to have an argument label.

[variadic-parameters]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID171
