## String Indexing

In many languages, strings are implemented as or treated as arrays of characters, with access to individual characters made possible through indexing, just like with arrays. For example, given a variable `csv` holding the string `"apple,pear,peach,orange,cherry,lime,goosberry"` ,`csv[21]` would return the character `"g"` from "orange".

In Swift, individual characters can be accessed using a subscripting method that looks similar to that used with arrays. However, the indices used for strings are _not_ integers and can not be worked with directly.

Instead they must be computed based off of some other known index, such as `startIndex`, which points to the position of the first character in a nonempty string, using the methods available in the [`String`][string-docs] and [`NSString`][nsstring-docs] libraries (when the `Foundation` module is imported, strings in Swift have access to all of the NSString properties and methods).

For example, you cannot write `csv[21]` to get the "g" in "orange", you must instead compute a value of type `String.Index`. Note that these indices are not meant to be human-consumable on their own. They are what is referred to as _opaque indices_ as humans are not meant to concern themselves with what is inside them.

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

All of this is rather surprising (and irritating) to programmers who come to Swift from languages where array-like access is the norm. But Swift takes a different view of strings and once one becomes used to the prefix and suffix based ways of consuming Strings in Swift, string consumption begins to fell natural again.

[string-docs]: https://developer.apple.com/documentation/swift/String
[nsstring-docs]: https://developer.apple.com/documentation/foundation/nsstring
