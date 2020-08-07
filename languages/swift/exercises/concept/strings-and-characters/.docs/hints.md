## 1. Create a set of useful strings

- [Strings][strings-and-characters] are defined as literals by enclosing zero or more of characters between double quotation marks (`"`).

## 2. Create a set of useful characters

- [Characters][strings-and-characters] are defined as literals by enclosing exactly one character between double quotation marks (`"`).
- Characters defined as literals need to be explicitly given a `Character` type annotation to prevent them from being interpreted as Strings by the compiler.

## 3. Combine phrases to build up messages

- Strings can be concatenated using the `+` operator.
- Characters cannot be concatenated with Strings. They must first be converted into Strings.

## 4. Build a graduation sign

- [String interpolation][string-interpolation] is performed by placing the expression whose value you want to place in the string inside a set of parentheses which are preceded by a backslash character.

## 5. Count the number of lines in a sign's message

- One can iterate over the characters of a string using a for-in loop.
- Characters and strings can be [compared for equality][string-equality] with the `==` operator.

## 6. Count the number of numbers in a sign's message

- One can iterate over the characters of a string using a for-in loop.
- One can determine if a character is a number by [checking its numeric properties][character-docs].

[strings-and-characters]: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html
[character-docs]: https://developer.apple.com/documentation/swift/character
[string-interpolation]: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292
[string-equality]: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID299
