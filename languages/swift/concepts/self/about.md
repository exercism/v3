Instances of structs and classes each have an implicit value named `self` which refers to the instance itself. There are multiple uses for `self`, but it is most commonly used to disambiguate the names of properties and methods of the struct/class when there may be some confusion.

```swift
struct MySelf {
  var x = 0

  mutating func move(x: Int) {
    // here if we just say x = x it is unclear if we mean
    // the property x or the method parameter x, so we use
    // self for clarity
    self.x = x
  }
}
```

Structs and classes also have a `Self` value which refers the the type if the struct or class, rather than the instance of the struct or class.
