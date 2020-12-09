Individual characters in a `String` can be accessed using the subscripting method used with arrays. However, the indices used for strings are _not_ integers and can not be worked with directly. Instead they must be computed based off of some other known index, such as `startIndex`, which points to the position of the first character in a nonempty string, using the methods available in the [`String`][string-docs] and [`NSString`][nsstring-docs] libraries (when the `Foundation` module is imported, strings in Swift have access to all of the NSString properties and methods).

For example, given the following string:

```swift
let csv = "apple,pear,peach,orange,cherry,lime,goosberry"
```

One cannot write `csv[21]` to get the "g" in "orange", they must instead compute a value of type `String.Index` and supply that index instead. Note that these indices are not meant to be human-consumable on their own. They are what is referred to as _opaque indices_ ,as humans need not know what is inside them.

```swift
let index = csv.index(csv.startIndex, offsetBy: 21)
csv[index]
// => "g"
print(index)
// => Index(_rawBits: 1376513)
```

Note, however, that if the offset is not a valid index, i.e. if it would return the index before `startIndex` or after `endIndex` the operation will raise an error, crashing the program. To prevent this problem, one can specify a limiting index. This returns an optional index and it will return nil for otherwise invalid indices.

```swift
let tooFar = csv.index(csv.startIndex, offsetBy: 200)
// => error: Execution was interrupted, reason: EXC_BAD_INSTRUCTION (code=EXC_I386_INVOP, subcode=0x0).
let tooFarButSafe = csv.index(csv.startIndex, offsetBy: 200, limitedBy: csv.endIndex)
// => nil
```

Additionally, indices cannot be shared between strings. For example, using the `index` of the "g" in "orange" computed above to index into the string `fam = "This is my family: 👨‍👩‍👦‍👦, this is our dog: 🐶, this is our cat: 🐱!"`, i.e. `fam[index]` will crash your program.

There are many reasons for all of this, but they all basically boil down to "Unicode is tricky".

Generally speaking if you need random access to individual characters, you likely want to be using some other data structure, like `Array<Character>`.

[string-docs]: https://developer.apple.com/documentation/swift/String
[nsstring-docs]: https://developer.apple.com/documentation/foundation/nsstring
[string-format-specifiers]: https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Strings/Articles/formatSpecifiers.html
[string-interpolation]: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292
