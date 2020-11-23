[While loops][while-loops] in Swift have the following structure:

```swift
while boolean-expression {
  loop-body
}
```

The loops begin by first evaluating a Boolean expression. If the expression evaluates to `false`, then the body of the loop is skipped and execution begins on the first line following the loop. If the condition evaluates to `true`, then the body of the loop is executed, after which the Boolean expression is evaluated again, repeating this way until the Boolean expression evaluates to false.

```swift
var count = 3
while count > 0 {
  print("\(count)…")
  count -= 1
}
print("Liftoff!")

// prints:
// 3…
// 2…
// 1…
// Liftoff!
```

### Optional binding loops

One common variant of while loops in Swift is the optional binding while loop. These loops repeat as long as the call to some function that yields an optional value returns a non-`nil` value. That (non-optional) value is then boud to a name of your choice and can be used in the body of the loop.

```swift
var arr = [1,2,3]
while let count = arr.popLast() {
  print("\(count)…")
}
print("Liftoff!")

// prints:
// 3…
// 2…
// 1…
// Liftoff!
```

[while-loops]: https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html#ID124
