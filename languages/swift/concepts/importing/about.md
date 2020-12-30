While Swift includes a lot of functionality in its Standard Library, there is much more functionality available in the form of external libraries. These libraries may be from the Swift project itself, such as the [Swift Argument Parser][argument-parser], closed source libraries from companies Apple's [Network Framework][network-framework], or third-party libraries, like [Swifty Beaver][swifty-beaver].

Some of these modules, like the Network Framework or [Foundation][apple-foundation] (which is probably the most commonly used library in Swift) come with your Swift distribution. External third party libraries need to be added to your project before they can be imported, though that is out of the scope of this exercise.

Importing modules is done by writing the `import` keyword followed by the name of the module. So one can import Foundation by adding the following to their program.

```swift
import Foundation
```

This allows access to all of the types, values, and functionality inside that module; for example if one wishes to use the `components(separatedBy:)` String method, that method becomes available to `String` with this import.

While they can be placed in the code anywhere before a pice of code that makes use of one the content of module, import statements are usually placed at the beginning of the file that uses them for greater readability.

[argument-parser]: https://github.com/apple/swift-argument-parser
[network-framework]: https://developer.apple.com/documentation/network
[swifty-beaver]: https://github.com/apple/swift-argument-parser
[apple-foundation]: https://developer.apple.com/documentation/foundation
