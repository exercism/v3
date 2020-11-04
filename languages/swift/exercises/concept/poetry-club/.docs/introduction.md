## importing
## opaque-indices
## string-components
## string-indexing

We've seen how to build up strings in previous exercises. Here we will see some of the ways to break strings apart.

### Breaking strings apart

In addition to building up strings out of components, strings may also be taken apart and broken into individual components. This is useful for tasks such as breaking apart a list of comma separated values into an array of the individual entities. Note that not all of these functions are in Swift's standard library and will require the importing of additional libraries. The most commonly used library is `Foundation`.

Importing modules is done by writing the `import` keyword followed by the name of the module. This allows access to all of the types, values, and functionality inside that module; in this example we are making use of the `components(separatedBy:)` method which becomes available to `String` with this import.

```swift
import Foundation

let csv = "apple,pear,peach,orange,cherry,lime,goosberry"
let fruit = csv.components(separatedBy: ",")
// => ["apple", "pear", "peach", "orange", "cherry", "lime", "goosberry"]
```

### Accessing individual characters

Accessing individual characters in strings is both like and unlike accessing individual elements of an array. The first and last elements are easily accessed using the `first` and `last` properties of the string respectively. Note that, as a string may not have a first or last character, these properties return optional Characters which will need to be unwrapped if they are to be used.

Other individual characters can be accessed using the subscripting method used with arrays. However, the indices used for strings are _not_ integers and can not be worked with directly. Instead they must be computed based off of some other known index, such as `startIndex`, which points to the position of the first character in a nonempty string.

For example, you cannot write `csv[21]` to get the "g" in "orange", you must instead compute a value of type `String.Index`.

```swift
let index = csv.index(csv.startIndex, offsetBy: 21)
csv[index]
// => "g"
```

The most basic indices associated with strings are the start and end indices which point to the first character and the _position after_ the last character in a string. These indices are always guaranteed to be valid indices, but they may not necessarily point to valid characters in a string. For example, since `endIndex` is the index that points to the position _after_ the last character, trying to subscript the string at that position will raise an error.

Note, however, that if the offset is not a valid index, i.e. if it would return the index before `startIndex` or after `endIndex` the operation will raise an error, crashing the program. To prevent this problem, one can specify a limiting index. This returns an optional index and it will return `nil` for otherwise invalid indices.
