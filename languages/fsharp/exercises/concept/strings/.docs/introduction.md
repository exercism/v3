## strings

A `string` in F# is an object that represents immutable text as a sequence of Unicode characters (letters, digits, punctuation, etc.) and is defined as follows:

```fsharp
let fruit = "Apple"
```

Strings are manipulated by either calling the string's methods, or using the `String` module's functions. Once a string has been constructed, its value can never change. Any methods/functions that appear to modify a string will actually return a new string.
