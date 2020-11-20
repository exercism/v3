We've seen how to build up strings in previous exercises. Here we will see some of the ways to break strings apart.

### Breaking strings apart

In addition to building up strings out of components, strings may also be taken apart and broken into individual components. This is useful for tasks such as breaking apart a list of comma separated values into an array of the individual entities. Note that not all of these functions are in Swift's standard library and will require the importing of additional libraries. In this example we are making use of the `components(separatedBy:)` method which requires importing the `Foundation` module. Once `Foundation` is imported, this method and many others become available to the `String` type.

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

### Accessing internal characters

In Swift, strings are treated more as something that is to be traversed, rather than randomly accessed, though that can be done. There are two main reasons for this:

1. Swift strings are built with unicode in mind, and you don't know ahead of time how much space any unicode character will take up, this is especially the case when emoji come into play, so using random-indexing is either verbose or slow or both.
2. Most real world string processing is done by traversing strings and not randomly accessing bits of them.

Because of this, most operations on Strings are done by pulling characters off the front or back of a String using operations like `prefix(_:)`, `suffix(_:)`, `dropFirst(_:)`, `dropLast(_:)`, etc. (these methods are discussed in the [`String` documentation][string-docs]).

So, for example, to get the first three characters of a string, rather than write `myString[0...2]`, as one might with an `Array`, they would instead write `myString.prefix(3)`. And to get the second three, they may write something like `myString.dropFirst(3).prefix(3)` or `myString.prefix(6).suffix(3)`. There are a few ways to do things operations like that and they have different benefits and downsides.

Trying to use indices and index manipulation with strings is simply fighting against the design of Swift and often leads to more problems than it solves.

[string-docs]: https://developer.apple.com/documentation/swift/String
