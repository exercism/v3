## Multiple return values

[Multiple values can be returned][multiple-return-values] from Swift functions by creating and returning a tuple from the different values.

```swift
func reverseAndLength(_ str: String) -> (reverse: String, length: Int) {
  return (reverse: str.reverse, length: str.count)
}

reverseAndLength("Hello")
// => (reverse: "olleH", length: 5)
```

## Omitting the return

```swift
func reverseAndLength(_ str: String) -> (reverse: String, length: Int) {
  (reverse: str.reverse, length: str.count)
}
```

In cases where the entire body of a function is a single expression, [the `return` keyword may be omitted][implicit-returns].

Note that this applies for all return types, not just multiple-value returns.

[multiple-return-values]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID164
[implicit-returns]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID607
