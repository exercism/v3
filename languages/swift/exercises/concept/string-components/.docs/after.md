We've seen how to build up strings in previous exercises. Here we will see some of the ways to break strings apart.

### Breaking strings apart

In addition to building up strings out of components, strings may also be taken apart and broken into individual components. This is useful for tasks such as breaking apart a list of comma separated values into an array ofthe individual entities.

```swift
import Foundation

let csv = "apple,pear,peach,orange,cherry,lime,goosberry"
let fruit = csv.components(separatedBy: ",")
// => ["apple", "pear", "peach", "orange", "cherry", "lime", "goosberry"]
```

### Accessing individual characters

Accessing individual characters in strings is both like and unlike accessing individual elements of an array. The first and last elements are easily accessed using the `first` and `last` methods respectively. Note that, as a string may not have a first or last character, these properties return optional Characters which will need to be unwrapped if they are to be used.

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

Other individual characters can be accessed using the subscripting method used with arrays. However, the indices used for strings are _not_ integers and can not be worked with directly. Instead they must be computed based off of some other known index using the methods available in the [`String`][string-docs] and [`NSString`][nsstring-docs] libraries (when the `Foundation` module is imported, strings in Swift have access to all of the NSString properties and methods).

For example, you cannot write `csv[11]` to get the "g" in "orange", you instead have to write something like:

```swift
let index = csv.index(csv.startIndex, offsetBy: 19)
csv[index]
// => "g"
```

Note, however, that if the offset is not a valid index, i.e. if it would return the index before `startIndex` or after `endIndex` the operation will raise an error, crashing the program. To prevent this problem, one can specify a limiting index. This returns an optional index and it will return nil for otherwise invalid indices.

```swift
let tooFar = scal.index(csv.startIndex, offsetBy: 200)
// => error: Execution was interrupted, reason: EXC_BAD_INSTRUCTION (code=EXC_I386_INVOP, subcode=0x0).
let tooFarButSafe = scal.index(scal.startIndex, offsetBy: 200, limitedBy: scal.endIndex)
// => nil
```

Additionally, indices cannot be shared between strings. For example, using the `index` of the "g" in "orange" computed above to index into the string `fam = "This is my family: 👨‍👩‍👦‍👦, this is our dog: 🐶, this is our cat: 🐱!"`, i.e. `fam[index]` will crash your program.

There are many reasons for all of this, but they all basically boil down to "Unicode is tricky".

Generally speaking if you need random access to individual characters, you likely want to be using some other data structure, like `Array<Character>`.

[string-docs]: https://developer.apple.com/documentation/swift/String
[nsstring-docs]: https://developer.apple.com/documentation/foundation/nsstring
[string-format-specifiers]: https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Strings/Articles/formatSpecifiers.html
[string-interpolation]: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292
