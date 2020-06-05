- [Chars documentation][chars-docs]: reference documentation for `char`.
- [Chars tutorial][chars-tutorial]: basic tutorial on how to work with chars.

`char`s are generally easy to use. They can be extracted from strings, added back
(by means of a string builder), initialised using literals, assigned and compared.
However, there are a number of rough edges as detailed below.

These rough edges mostly relate to the conflict
between the full unicode standard on the one side and historic representations
of text as well as performance and memory usage on the other.

### Unicode Issues

You should always try to use library methods such as `IsControl`, `IsDigit`
rather than making naive comparisons such as checking that a character is
between '0' and '9'. For instance, note that '٢' is the arabic digit 2. `IsDigit`
will return true for the arabic version so you need to be clear say when validating
what range of inputs is acceptable.

When dealing with strings, if `System.String` library methods are available you should
seek these out and use them rather than breaking the string down into characters.
Some textual "characters" consist of more than one `char` because the unicode standard
has more than 65536 code points. For instance the emojis that show up in some
of the tests comprise 2 `char`s as they are "surrogates".

The safest way to break a string into "characters" is to use [`StringInfo`][string-info] and
methods such as `GetNextTextElement`. However, if you know the range of characters
you deal with does not include surrogates or combining characters (e.g. Latin ASCII) and your input
is well validated then you can avoid this. Again, the best position to be in
is where you can use `String`'s library methods.

### Globalization

If you are working in an environment where you are dealing with multiple cultures or
the culture is important in some parts of the code but not others then be
aware of the overloads of `ToUpper` and `ToLower` which take a culture and
`ToUpperInvariant` and `ToLowerInvariant` which will provide a consistent
result irrespective of the current [culture][culture-info].

### Representation, Chars and Integers

Like other simple types (`int`s, `bool`s, etc.) the `char` has a companion
or alias type, in this case, `System.Char`. This is in fact a `struct` with
a 16 bit field. `char` in fact has some instance methods such as
`Equals`, `ToString` and `CompareTo`.

`char` has the same width as a [`ushort`][uint16] but they are generally
not used inter-changeably as they are in some languages. `ushort` has
to be explicitly cast to a `char`. For what it's worth `char`s can
be subject to arithmetic operations provided the left hand side is an integer.

Obviously there is no equivalence between a `byte` at 8 bits and the 16 bit `char`.

### Performance

Using `StringBuilder` is seen as hugely preferable to concatenating strings with
a `+` or `+=` operator. This is for performance reasons mostly related
to the cost of memory allocation. There are edge cases
where it might underperform concatenation but the difference is likely to be trivial
and can generally be ignored.

[chars-docs]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/char
[chars-tutorial]: https://csharp.net-tutorials.com/data-types/the-char-type/
[culture-info]: https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=netcore-3.1
[uint16]: https://docs.microsoft.com/en-us/dotnet/api/system.uint16?view=netcore-3.1
[string-info]: https://docs.microsoft.com/en-us/dotnet/api/system.globalization.stringinfo?view=netcore-3.1
