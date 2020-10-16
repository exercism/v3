The key thing to remember about F# strings is that they are immutable objects representing text as a sequence of Unicode characters (letters, digits, punctuation, etc.). Double quotes are used to define a `string` instance:

```fsharp
let fruit = "Apple"
```

Manipulating a string can be done by calling one of its [methods][methods] or [properties][properties], or using one of the functions in the [`String` module][string-module]. As string values can never change after having been defined, all string manipulation methods/functions will return a new string.

A string is delimited by double quote (`"`) characters. Some special characters need [escaping][escaping] using the backslash (`\`) character. Strings can also be prefixed with the at (`@`) symbol, which makes it a [verbatim string][verbatim] that will ignore any escaped characters.

```fsharp
let escaped = "c:\\test.txt"
let verbatim = @"c:\test.txt"
escaped = verbatim
// => true
```

Alternatively, strings can be defined using [triple quotes][triple-quoted] (`"""`), which allows using a double quote in the string without escaping it.

```fsharp
let tripledQuoted = """<movie title="Se7en" />"""
// => "<movie title="Se7en" />
```

Finally, concatenating strings can be done through the [`+` operator][string_concatenation]:

```fsharp
let name = "Jane"
"Hello" + name + "!"
// => "Hello Jane!"
```

For any string formatting more complex than simple concatenation, the [`sprintf` function][string_concatenation] is preferred.

```fsharp
let name = "Jane"
sprintf "Hello %s!" name
// => "Hello Jane!"
```

[verbatim]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/strings#verbatim-strings
[triple-quoted]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/strings#triple-quoted-strings
[methods]: https://docs.microsoft.com/en-us/dotnet/api/system.string?view=netcore-3.1#methods
[properties]: https://docs.microsoft.com/en-us/dotnet/api/system.string?view=netcore-3.1#properties
[string-module]: https://msdn.microsoft.com/visualfsharpdocs/conceptual/core.string-module-%5bfsharp%5d
[escaping]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/strings#remarks
[string_concatenation]: https://exercism.github.io/v3/#/languages/fsharp/docs/string_concatenation
