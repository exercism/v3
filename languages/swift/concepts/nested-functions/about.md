## Nested functions

Functions may be defined [inside of other functions][nested-functions]. This is commonly used to create helper functions which are only useful to their enclosing function and so don't need to pollute the outside namespace.

These functions are defined and called just like normal functions, but are not visible outside the enclosing function.

```swift
func makeNumber(_ bits: [Bool]) -> Int {
  func double(_ x: Int) -> Int { 2 * x }
  func add(_ x: Int) -> Int { x + 1 }

  var number = 0
  for bit in bits {
    number = double(number)
    if bit {
      number = add(number)
    }
  }
  return number
}

makeNumber([true, false, true, true])
// => 11
makeNumber([true, true, false, false, false, true, true])
// => 99
```

[nested-functions]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID178
