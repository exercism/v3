[Strings][strings-and-characters] in Swift are a collection of characters, where characters are, more or less, a single printable element. So strings in swift are able to contain Unicode characters and emoji.

Strings are easily created in Swift through string literals. String literals are similar to those seen in many other programming languages, characters enclosed between a pair of double quotation marks (`"`). String literals can include the following special characters:
`\0` (null character), `\\` (backslash), `\t` (horizontal tab), `\n` (newline), `\r` (carriage return), `\"` (double quotation mark) and `\'` (single quotation mark) String literals can also contain arbitrary Unicode scalar values, written as\u{_n_}, where _n_ is a 1–8 digit hexadecimal number.

An empty string is represented by two double quotation marks with nothing between them.

```swift
let hello = "Hello, World!"
let sausage = "Weißwurst is a tasty sausage."
var fam = "This is my family: 👨‍👩‍👦‍👦, this is our dog: 🐶"
var empty = ""
```

Character literals in Swift look just like string literals, however, only one character is allowed between the quotation marks. Since the two literals look the same, Swift will default to inferring that characters are strings unless the type is specified by an explicit annotation or the context can tell the compiler otherwise.

```swift
let aString = "A"
// Swift infers this to be a String

var aChar: Character = "A"
// Swift now knows to make this a Character

let badChar: Character = "Too many characters"
// Error: Cannot convert value of type 'String' to specified type 'Character'
```

Like other constructs in Swift, characters and strings are immutable if defined using `let` and mutable if defined using `var`. So in the above examples, `hello` and `sausage` are immutable strings while `fam` and `empty`, and `aChar` are mutable.

### Type conversion

Type conversion allows us to get the string representation of many different types in Swift, including `Int`s, `Double`s and `Character`s.

```swift
String(110)
// => "110"
String(Double.pi)
// => "3.141592653589793"
let charX: Character = "x"
String(charX)
// "x"
```

Some of the conversions, like those for `Int` allow for different equivalent representations:

```swift
String(113, radix: 2)
// => "1110001"
String(3735928559, radix: 16)
// => "deadbeef"
```

### Building strings from smaller parts

A character or string can be appended to a string through the string's `append(_:)` method:

```swift
var greeting = "Hello"
let world = ", world"
let period: Character = "."
greeting.append(world)
// greeting is now: "Hello, world"
greeting.append(period)
// greeting is now: "Hello, world."
```

Strings can be concatenated using the `+` operator, and mutable strings can have other strings appended onto them using the `+=` mutating operator.

```swift
"honey" + "comb"
// => "honeycomb"

fam += ", this is our cat: 🐱"
// => "This is my family: 👨‍👩‍👦‍👦, this is our dog: 🐶, this is our cat: 🐱"
```

Though this can have unexpected effects when Unicode characters are in play.

```swift
let zeroWidthJoiner = "\u{200D}"
"🏳️" + zeroWidthJoiner + "🌈"
 // => "🏳️‍🌈"
```

Note that only two strings can be combined this way. In order to use `+` with a Character, it must first be converted to a String:

```swift
let question: Character = "?"
let areYouHappy = "Happy" + question
// Error: Cannot convert value of type 'Character' to expected argument type 'String'

let areYouHappy = "Happy" + String(question)
// => "Happy?"
```

Strings may also be built up out of values of other types in two other ways: format strings, or string interpolation.

### Format strings

Format strings are similar to those seen in other languages, where a [string with special tokens inside][string-format-specifiers] is followed by a list of components that are inserted into the string at the location of the tokens, formatted as directed by the tokens:

```swift
String(
  format: "The unsigned decimal integer %u is 0x%08X in hexadecimal",
  3735928559,
  3735928559
)
// => "The unsigned decimal integer 3735928559 is 0xDEADBEEF in hexadecimal"
```

### String interpolation

The most common way to build up strings in Swift is [_string interpolation_][string-interpolation]. String interpolation is similar in spirit to format strings, though less fussy and with somewhat less control over the appearance of the string. To insert a value into a string using string interpolation, you place an expression or value in parentheses which are preceded by a backslash character.

```swift
let radius = 5.0
let interp = "The area of a circle with radius \(radius) is \(Double.pi * radius * radius)"
// => "The area of a circle with radius 5.0 is 78.53981633974483"
```

As noted above, string interpolation will insert the default string representation of the given value into the constructed string (this is the same as what is output if one uses `print(value)`), and this may not be what is wanted or expected. Any additional formatting will need to be applied to the value itself, before string interpolation can build a string out of it. With Swift 5, it is now possible to add custom formatting by extending string interpolation, but that is well beyond the scope of this exercise.

### String and Character equality

Strings and Characters can be compared for equality (or lack thereof) using the `==` operator (or `!=` for not-equal).

```swift
let hi = "Hello"
hi == "Hello."
// => true

let period: Character = "."
period != ";"
// => true
```

Note that, with Unicode, there are a number of ways to represent some symbols. For example the letter é can be represented as `"\u{E9}"` and as `"\u{65}\u{301}"`. Swift considers two different strings or characters to be equal ["if they have the same linguistic meaning and appearance, even if they’re composed from different Unicode scalars behind the scenes."][string-equality] So the two representations of the letter é are considered to be equal in Swift, even if they look very different.

The reverse can also be true. For example, the characters "В" and "B" look the same, but are not considered equal in Swift, as the first one is the Cyrillic letter "В" which has a different linguistic meaning than the second character, which is the Latin letter "B", despite their having the same appearance.

### String and Character properties

Strings and characters have a few different _properties_ which can be queried to get information about the string or character. These are queried by placing a ._propertyName_ after the string or character in question For example, one can check to see if a string is empty by querying it's `isEmpty` property, and the count of characters in a string can be retrieved using its `count` property.

Character properties are mostly based on the type of character it is. For example, one can check to see if a character `isNumber`, `isLetter`, `isUppercase`, etc.

```swift
empty.isEmpty
// => true
fam.count
// => 60
aChar.isLowercase
// => false
let half: Character = "½"
half.isNumber
// => true
```

All of the [string properties][string-docs] and [character properties][character-docs] can be found in Apple's documentation.

## NSString

Strings in Swift also obtain all of the functionality of the [NSString][nsstring-docs] class which was originally used in Apple's Objective-C libraries.

[strings-and-characters]: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html
[string-docs]: https://developer.apple.com/documentation/swift/String
[nsstring-docs]: https://developer.apple.com/documentation/foundation/nsstring
[character-docs]: https://developer.apple.com/documentation/swift/character
[string-format-specifiers]: https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Strings/Articles/formatSpecifiers.html
[string-interpolation]: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292
[string-equality]: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID299
