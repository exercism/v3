### General

- Read about strings in the official [Getting Started Elixir guide][getting-started-strings].
- Browse the [functions available in the _String module_][string-module-functions] to discover which operations on strings Elixir's standard library offers.

### 1. Get the name's first letter

- Use [`String.first/1`][string-first] to get the first character from a string.
- Use [`String.trim/1`][string-trim] to remove leading and trailing whitespace from a string.

### 2. Format the first letter as an initial

- Use [`String.upcase/1`][string-upcase] to convert all characters in a string to their uppercase variant.
- Use `<>/2` to concatenate two strings.

### 3. Split the full name into the first name and the last name

- Use [`String.split/1`][string-split] to split a string on whitespace characters.
- Use pattern-matching on the value returned by `String.split/1`. It will return a two-element list.

### 4. Put the initials inside of the heart

- Use the `#{}` syntax to [interpolate][string-interpolation] an expression inside of a string.
- Use the heredoc syntax with triple-double-quotes `"""` to comfortably work with multiline strings.

[getting-started-strings]: https://elixir-lang.org/getting-started/basic-types.html#strings
[string-module-functions]: https://hexdocs.pm/elixir/String.html#functions
[string-first]: https://hexdocs.pm/elixir/String.html#first/1
[string-trim]: https://hexdocs.pm/elixir/String.html#trim/1
[string-upcase]: https://hexdocs.pm/elixir/String.html#upcase/2
[string-split]: https://hexdocs.pm/elixir/String.html#split/1
[string-interpolation]: https://hexdocs.pm/elixir/String.html#module-interpolation
