## Importing Libraries

In Swift, the only library of functionality that is automatically made available for your program to use is the Swift Standard Library. However, there are many other libraries that are available in Swift that offer up a huge amount of functionality. There are libraries from the Swift team, such as [Argument Parser][swift-argument-parser] and [Swift Algorithms][swift-algorithms], some from Apple in closed-source form, such as [SwiftUI][swift-ui] and [Network Framework][swift-network], and others from third parties, liike [Swifty Beaver][swifty-beaver] and [AlamoFire][alamo-fire]. Perhaps the most commonly used library in Swift is Apple's [Foundation][swift-foundation], which contains a huge amount of basic functionality needed for programming.

However, in order to use these libraries, they must first be imported into your program.

Importing modules is done by writing the `import` keyword followed by the name of the module. This allows access to all of the types, values, and functionality inside that module; in this example we are making use of the `components(separatedBy:)` method which becomes available to `String` with this import. Some of these modules, like `Foundation` come with your Swift distribution. External third party libraries need to be added to your project before they can be imported, though that is out of the scope of this exercise.

```swift
import Foundation

let csv = "apple,pear,peach,orange,cherry,lime,goosberry"
let fruit = csv.components(separatedBy: ",")
// => ["apple", "pear", "peach", "orange", "cherry", "lime", "goosberry"]
```

[swift-argument-parser]: https://github.com/apple/swift-argument-parser
[swift-algorithms]: https://github.com/apple/swift-algorithms
[swift-foundation]: https://developer.apple.com/documentation/foundation
[swift-ui]: https://developer.apple.com/documentation/swiftui
[swift-network]: https://developer.apple.com/documentation/network
[swifty-beaver]: https://github.com/SwiftyBeaver/SwiftyBeaver
[alamo-fire]: https://github.com/Alamofire/Alamofire
