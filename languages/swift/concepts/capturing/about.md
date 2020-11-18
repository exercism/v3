In Swift, closures and functions (which are just special cases of closures) are able to access the parameters and variables of their surrounding environment. Additionally, they are able to maintain access to these values after the enclosing function terminates. This action of obtaining and maintaining access is known as [_capturing_][capturing-values].

```swift
 func makeAdder(base: Int) -> (Int) -> Int {
   func adder(_ i: Int) -> Int {
     base + i
   }
   return adder
 }
 // makeAdder: (Int) -> (Int) -> Int

 let add10 = makeAdder(base: 10)
 // add10: (Int) -> Int

 let subtract20 = makeAdder(base: -20)
 // subtract10: (Int) -> Int

 add10(5)
 // => 15

 subtract20(5)
 // => -15

 func makeLogger(logLevel: String) -> (String) -> () {
   let logHeader = "[\(logLevel)]: "
   return { msg in print(logHeader, msg) }
 }
 // makeLogger: (String) -> (String) -> ()

 let infoLogger = makeLogger("info")
 // infoLogger: (String) -> ()

 infoLogger("This is a log message")
 // prints "[info]: This is a log message"
```

This capturing is seen with the `makeAdder(base:)` and `makeLogger(logLevel:)` function above where the returned function and closure and continue to use the captured values of `base` and `logHeader` even after the `makeAdder(base:)` and `makeLogger(logLevel:)` have finished executing.

[capturing-values]: https://docs.swift.org/swift-book/LanguageGuide/Closures.html#ID103
