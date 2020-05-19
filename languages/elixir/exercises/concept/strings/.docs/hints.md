### General

- Read about strings in the official [Getting Started Elixir guide][getting-started-strings].
- Browse the [functions available in the _String module_][string-module-functions] to discover which operations on strings Elixir's standard library offers.

### 1. Get the name's first letter

- There are a few different ways to get a specific character or characters from a string based on their index: [`String.at/2`][string-at], [`String.slice/3`][string-slice], [`String.first/1`][string-first], [`String.last/1`][string-last]
- There are a few different ways to remove unwanted whitespace from a string: [`String.trim/1`][string-trim], [`String.trim_leading/1`][string-trim-leading], [`String.trim_trailing/1`][string-trim-trailing].

### 2. Format the first letter as an initial

- There are a few different ways to change the case of a string: [`String.downcase/2`][string-downcase], [`String.upcase/2`][string-upcase], [`String.capitalize/2`][string-capitalize].
- Two strings can be concatenated using `<>/2`.

### 3. Split the full name into the first name and the last name

- Strings can be split on whitespace characters with [`String.split/1`][string-split1] or on a specific character with [`String.split/2`][string-split2].
- A few first elements of a list can be assigned to variables by pattern-matching on the list.

### 4. Put the initials inside of the heart

- The `#{}` syntax can be used to [interpolate][string-interpolation] an expression inside of a string.
- Newlines do not need to be escaped when using the [triple-double-quote heredoc syntax][heredoc-syntax].

[getting-started-strings]: https://elixir-lang.org/getting-started/basic-types.html#strings
[string-module-functions]: https://hexdocs.pm/elixir/String.html#functions
[string-first]: https://hexdocs.pm/elixir/String.html#first/1
[string-last]: https://hexdocs.pm/elixir/String.html#last/1
[string-at]: https://hexdocs.pm/elixir/String.html#at/2
[string-slice]: https://hexdocs.pm/elixir/String.html#slice/3
[string-trim]: https://hexdocs.pm/elixir/String.html#trim/1
[string-trim-leading]: https://hexdocs.pm/elixir/String.html#trim_leading/1
[string-trim-trailing]: https://hexdocs.pm/elixir/String.html#trim_trailing/1
[string-upcase]: https://hexdocs.pm/elixir/String.html#upcase/2
[string-downcase]: https://hexdocs.pm/elixir/String.html#downcase/2
[string-capitalize]: https://hexdocs.pm/elixir/String.html#capitalize/2
[string-split1]: https://hexdocs.pm/elixir/String.html#split/1
[string-split2]: https://hexdocs.pm/elixir/String.html#split/2
[string-interpolation]: https://hexdocs.pm/elixir/String.html#module-interpolation
[heredoc-syntax]: https://elixir-examples.github.io/examples/multiline-strings-heredocs
