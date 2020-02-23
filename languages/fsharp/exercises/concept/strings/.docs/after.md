# After

The key thing to remember about F# strings is that they are immutable objects representing text as a sequence of Unicode characters (letters, digits, punctuation, etc.).

Manipulating a string can be done by calling one of its [methods][methods] or [properties][properties], or using one of the functions in the [`String` module][string-module]. As string values can never change after having been defined, all string manipulation methods/functions will return a new string.

A string is delimited by double quote (`"`) characters. Some special characters need [escaping][escaping] using the backslash (`\`) character. Strings can also be prefixed with the at (`@`) symbol, which makes it a [verbatim string][verbatim] that will ignore any escaped characters.

Alternatively, strings can be defined using triple quotes (`"""`), which allows using a double quote in the string without escaping it. [Triple-quoted strings][triple-quoted] can also span multiple lines.

Finally, concatenating strings can be done through the [`+` operator][plus-operator]. For any string formatting more complex than simple concatenation, the `sprintf` function is preferred.

[verbatim]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/strings#verbatim-strings
[triple-quoted]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/strings#triple-quoted-strings
[methods]: https://docs.microsoft.com/en-us/dotnet/api/system.string?view=netcore-3.1#methods
[properties]: https://docs.microsoft.com/en-us/dotnet/api/system.string?view=netcore-3.1#properties
[string-module]: https://msdn.microsoft.com/visualfsharpdocs/conceptual/core.string-module-%5bfsharp%5d
[escaping]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/strings#remarks
[plus-operator]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/strings#string-operators
