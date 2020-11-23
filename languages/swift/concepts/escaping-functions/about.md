### Escaping functions

There are times that a higher-order function takes another function as a parameter and uses it in a way that the passed-in function is called _after_ the higher-order function terminates. This is known as escaping the higher-order function, and the passed-in function is referred to as an [escaping function][escaping].

This happens most often in asynchronous code, but it can occur in higher-order functions where the passed-in function (or a function that calls it) is returned from the higher-order function. For example:

```swift
func emptyKitchen(_ order: String) -> String {
  "Sorry, we're all out of \(order)."
}

func prepare(order: String, kitchen: (String) -> String) -> (String) -> String {
  func newKitchen(_ newOrder: String) -> String {
    if newOrder == order {
      return "One \(order) coming up!"
    } else {
      return kitchen(newOrder)
    }
  }
  return newKitchen
}
```

Here, the function `prepare` accepts a function for its `kitchen` parameter, then constructs a new function `newKitchen` which may call `kitchen`. This `newKitchen` function is then returned to the caller. Trying to write this function in Swift results in the error: _Escaping local function captures non-escaping parameter 'kitchen'_

Swift raises an error in this case because there are situations where the nested or passed-in function may capture a value that can lead to memory leaks. How this happens and how to prevent these leaks are beyond the scope of this exercise. However, this error can be satisfied by placing the `@escaping` attribute before the passed-in function's type signature.

```swift
func prepare(order: String, kitchen: @escaping (String) -> String) -> (String) -> String {
  func newKitchen(_ newOrder: String) -> String {
    if newOrder == order {
      return "One \(order) coming up!"
    } else {
      return kitchen(newOrder)
    }
  }
  return newKitchen
}

let restaurant =
  prepare(order: "sandwich",
          kitchen: prepare(order: "chicken",
                           kitchen: prepare(order: "steak",
                                            kitchen: emptyKitchen)))

restaurant("pork chop")
// => "Sorry, we're all out of pork chop."
restaurant("chicken")
// => "One chicken coming up!"
```

This attribute signals to the Swift compiler that the author is aware that memory leaks may occur by allowing this function to escape.

[escaping]: https://docs.swift.org/swift-book/LanguageGuide/Closures.html#ID546
