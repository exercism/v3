We've seen how to build up strings in previous exercises. Here we will see some of the ways to break strings apart.

### Breaking strings apart

In addition to building up strings out of components, strings may also be taken apart and broken into individual components. This is useful for tasks such as breaking apart a list of comma separated values into an array of the individual entities. Note that not all of these functions are in Swift's standard library and will require the importing of additional libraries. The most commonly used library is `Foundation`.

Importing modules is done by writing the `import` keyword followed by the name of the module. This allows access to all of the types, values, and functionality inside that module; in this example we are making use of the `components(separatedBy:)` method which becomes available to `String` with this import. Some of these modules, like `Foundation` come with your Swift distribution. External third party libraries need to be added to your project before they can be imported, though that is out of the scope of this exercise.

```swift
import Foundation

let csv = "apple,pear,peach,orange,cherry,lime,goosberry"
let fruit = csv.components(separatedBy: ",")
// => ["apple", "pear", "peach", "orange", "cherry", "lime", "goosberry"]
```

### Accessing individual characters

Accessing individual characters in strings is both like and unlike accessing individual elements of an array. The first and last elements are easily accessed using the `first` and `last` properties of the string respectively. Note that, as a string may not have a first or last character, these properties return optional Characters which will need to be unwrapped if they are to be used.

```swift
let scal = "Supercalifragilisticexpialidocious"
let empty = ""

scal.first
// => Optional("S")
scal.last!
// => "s"
empty.first
// => nil
```

Other individual characters can be accessed using [String Indices][string-indices].

[string-indices]: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID534
