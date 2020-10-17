[Repeat-while loops][repeat-loops] are similar to while loops, however, these loops differ in that the Boolean expression appears, and is evaluated, _after_ the body of the loop is executed. As a result, these loops always execute at least once.

```swift
repeat {
  print("This runs at least once")
} while false
print("Loop done")

// prints:
// This runs at least once
// Loop done
```
[repeat-loops]: https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html#ID126
