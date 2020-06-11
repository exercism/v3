Strings in Swift are a series of characters, where characters are, more or less, a single printable element. So strings in swift are able to contain Unicode characters and emoji. Some examples are "Hello, World!", "Weißwurst is a tasty sausage.", and "This is my family: 👨‍👩‍👦‍👦, this is our dog: 🐶"

Strings are easily created in Swift through string literals. String literals are similar to those seen in many other programming languages, character literals enclosed between a pair of double quotation marks (`"`). String literals can include the following special characters:
\0 (null character), \\ (backslash), \t (horizontal tab), \n (line feed), \r (carriage return), \" (double quotation mark) and \' (single quotation mark)<!-- String literals can also contain arbitrary Unicode scalar values, written as \u{n}, where n is a 1–8 digit hexadecimal number -->.

An empty string is represented by two double quotation marks with nothing between them.

```swift
let hello = "Hello, World!"
let sausage = "Weißwurst is a tasty sausage."
var fam = "This is my family: 👨‍👩‍👦‍👦, this is our dog: 🐶"
var empty = ""
```

Like most other constructs in Swift, strings are immutable if defined using `let` and mutable if defined using `var`. So in the above example, `hello` and `sausage` are immutable strings while `fam` and `empty` are mutable.

### Building strings from smaller parts

Strings can be concatenated using the `+` operator, and mutable strings can have other strings tacked onto them using the `+=` mutating operator.

```swift
"honey" + "comb"
// => "honeycomb"

fam += ", this is our cat: 🐱"
// => "This is my family: 👨‍👩‍👦‍👦, this is our dog: 🐶, this is our cat: 🐱"
```

<!--Though this can have unexpected effects when Unicode characters are in play.

```swift
let zwj = "\u{200D}"
"🏳️" + zwj + "🌈"
 // => "🏳️‍🌈"
```-->

Strings may also be built up out of values of other types in one of three ways, type conversion, format strings, or string interpolation.

Type conversion is similar to what we've seen before with converting strings or doubles to ints, though conversion to strings offers a few more options.

```swift
String(110)
// => "110"
String(Double.pi)
// => "3.141592653589793"
String(113, radix: 2)
// => "1110001"
String(3735928559, radix: 16)
// => "deadbeef"
```

Format strings are similar to those seen in other languages, where a string with special tokens inside is followed by a list of components that are inserted into the string at thee location of the tokens, formatted as directed by the tokens:

```swift
String(
  format: "The unsigned decimal integer %u is 0x%08X in hexidecimal",
  3735928559,
  3735928559
)
// => "The unsigned decimal integer 3735928559 is 0xDEADBEEF in hexidecimal"
```

String interpolation is similar in spirit to format strings, though less fussy and with somewhat less control over the appearance of the string. To insert a value into a string using string interpolation, you place an expression or value in parentheses which are preceeded by a backslash character.

```swift
let radius = 5.0
let interp = "The area of a circle with radius \(radius) is \(Double.pi * radius * radius)"
// => "The area of a circle with radius 5.0 is 78.53981633974483"
```

<!--As noted above, string interpolation with insert the default string representation of the given value into the constructed string (this is the same as what is output if one uses `print(value)`), and this may not be what is wanted or expected. Any additional formatting will need to be applied to the value itself, before string interpolation can build a string out of it.-->

### Breaking strings apart

In addition to building up strings out of components, strings may also be taken apart and broken into individual components. This is useful for tasks such as breaking apart a list of comma separated values into an array ofthe individual entities.

```swift
import Foundation

let csv = "apple,pear,peach,orange,cherry,lime,goosberry"
let fruit = csv.components(separatedBy: ",")
// => ["apple", "pear", "peach", "orange", "cherry", "lime", "goosberry"]
```

### Accessing individual characters

Accessing individual characters in strings is both like and unlike accessing individual elements of an array. The first and last elements are easily accessed using the `first` and `last` methods respectively and other individual characters can be accessed using the subscripting method used with arrays. However, the indicies used for strings are _not_ integers and can not be worked with directly. Instead they must be computed based off of some other known index using the methods available in the [`String`][string-docs] and [`NSString`][nsstring-docs] libraries (when the `Foundation` module is imported, strings in Swift have access to all of the NSString properties and methods).

<!--For example, you cannot write `csv[11]` to get the "p" in "peach", you instead have to write something like:

```swift
let index = csv.index(csv.startIndex, offsetBy: 11)
csv[index]
// => "p"
```
There are many reasons for this, but they all basically boil down to "Unicode is tricky"

Generally speaking if you need random access to individual characters, you want to be using so -->

[string-docs]: https://developer.apple.com/documentation/swift/String
[nsstring-docs]: https://developer.apple.com/documentation/foundation/nsstring
