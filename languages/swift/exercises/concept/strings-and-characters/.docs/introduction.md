Strings in Swift are a collection of characters, where characters are, more or less, a single printable element. So strings in Swift are able to contain Unicode characters and emoji.

Strings are easily created in Swift through string literals. String literals are similar to those seen in many other programming languages, characters enclosed between a pair of double quotation marks (`"`). String literals can include the following special characters:
\\0 (null character), \\\\ (backslash), \\t (horizontal tab), \\n (newline), \\r (carriage return), \\" (double quotation mark) and \\' (single quotation mark).

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

Like other constructs in Swift, characters and strings are immutable if defined using `let` and mutable if defined using `var`. So in the above examples, `hello` and `sausage` are immutable strings while `fam` and `empty`, AND `aChar` are mutable.

### Building strings from smaller parts

Strings can be concatenated using the `+` operator, and mutable strings can have other strings appended onto them using the `+=` mutating operator.

```swift
"honey" + "comb"
// => "honeycomb"

fam += ", this is our cat: 🐱"
// => "This is my family: 👨‍👩‍👦‍👦, this is our dog: 🐶, this is our cat: 🐱"
```

Note that only strings can be concatenated with strings. In order to use concatenation with a character, one must convert it to a String first

```swift
let question: Character = "?"
let areYouHappy = "Happy" + question
// Error: Cannot convert value of type 'Character' to expected argument type 'String'

let areYouHappy = "Happy" + String(question)
// => "Happy?"
```

### String Interpolation

The most common way to build up strings in Swift is _string interpolation_. To insert a value into a string using string interpolation, you place an expression or value in parentheses which are preceded by a backslash character.

```swift
let radius = 5.0
let interp = "The area of a circle with radius \(radius) is \(Double.pi * radius * radius)"
// => "The area of a circle with radius 5.0 is 78.53981633974483"
```

### String and Character equality

Strings and Characters can be compared for equality (or lack thereof) using the `==` operator (or `!=` for not-equal).

```swift
let hi = "Hello"
let period: Character = "."
"\(hi)\(period)" == "Hello."
// => true

period != ";"
// => true
```

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

### Iterating over the characters of a string

The individual characters in a string can be stepped through one at a time using a for-in loop. This type of loop takes each character in the string, in order, and binds the character to a specified name for further processing inside the loop body. For example, to print out all of the capital letters in a string one can write:

```swift
for char in "Hello Борис. How is Наташа?" {
  if char.isUppercase {
    print(char)
  }
}

// prints out:
// H
// Б
// H
// Н
```
