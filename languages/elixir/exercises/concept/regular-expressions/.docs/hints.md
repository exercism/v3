## General

- Review regular expression patterns from the introduction
  - Remember, when creating the pattern a string, you must escape some characters.
- Review the Elixir documentation:
  - [Regex][regex-docs]
  - [Sigils: Regular Expressions][sigils-regex]
- Check out these great regular expression resources:
  - [Regular-Expressions.info][website-regex-info]
  - [Rex Egg: The world's most tyrannosauical regex tutorial][website-rexegg]
  - [RegexOne: Learn Regular Expressions with simple, interactive exercises.][website-regexone]
  - [Regular Expressions 101: an online regex sandbox][website-regex-101]
  - [RegExr: an online regex sandbox][website-regexr]

## 1. Write patterns for each numeric component

- Remember to return a string representing the regular expression pattern.
- Review how to create _character classes_ or use _shorthand character classes_.
- Review _quantifiers_.

## 2. Write patterns for the day and month names

- Review how to write a pattern to match _string literals_.
- Review _alternations_.

## 3. Capture the numeric and named date components

- Review how to write patterns for captures and named captures.

## 4. Compose the parts to capture from the whole date

- Remember, string interpolation may be used to join strings.

## 5. Match only the date

- Remembers, _anchors_ help to match the pattern to the **whole line**.
- String interpolation may be used in the regular expression sigil syntax.

[regex-docs]: https://hexdocs.pm/elixir/Regex.html
[sigils-regex]: https://elixir-lang.org/getting-started/sigils.html#regular-expressions
[website-regex-info]: https://www.regular-expressions.info
[website-rexegg]: https://www.rexegg.com/
[website-regexone]: https://regexone.com/
[website-regex-101]: https://regex101.com/
[website-regexr]: https://regexr.com/
[website-regex-crossword]: https://regexcrossword.com/
