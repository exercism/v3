### Trailing Closures

Swift offers one other variation of its syntax when dealing with closures, known as [trailing closures][trailing-closures]. Trailing closures arise when the last parameter in a function's list of parameters has a function type. In this case, if one is passing in a closure, the closure can be written _after_ the closing parenthesis and the argument label is omitted. Note that if there is only one parameter for the function, the parentheses may be omitted entirely.

```swift
func fetchURL(_ urlString: String,
              uponReceipt handleResult: (String) -> ()
             )  -> () {
  if let url = URL(string: urlString), let htmlString = fetch(url) {
    handleResult(htmlString)
  }
}

fetchURL("https://www.swift.org") { print($0) }
// prints the fetched html string

["apple", "ball", "carrot"].sorted { $0.count < $1.count }
```

### Multiple trailing closures

While earlier versions of Swift only supported the use of a single trailing closure, beginning with version 5.2.4, Swift offers support for multiple trailing closures.

If a function has more than one closures at the end of its parameter list, trailing closure syntax can be used to move all of them outside of the parentheses. However, only the first argument label is omitted, the rest must be labeled:

```swift
func fetchURL(_ urlString: String,
              uponReceipt handleResult: (String) -> (),
              onFailure handleFailure: () -> ()
             )  -> () {
  if let url = URL(string: urlString), let htmlString = fetch(url) {
    handleResult(htmlString)
  } else {
    handleFailure()
  }
}

fetchURL("https://www.swift.org") {
  print($0)
} onFailure: {
  print("Error: https://www.swift.org could not be fetched")
}
// prints the fetched html string on successful fetch or prints error message on failure
```

[trailing-closures]: https://docs.swift.org/swift-book/LanguageGuide/Closures.html#ID102
