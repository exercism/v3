Regular expressions are a powerful tool for string in Elixir. Regular expressions in Elixir follow the **PCRE** specification (**P**erl **C**ompatible **R**egular **E**xpressions). String patterns representing the regular expression's meaning are first compiled then used for matching all or part of a string. The `=~/2` operator is useful to perform a boolean match on a string. A brief overview of regular expressions follows:

To match _literal characters_, you can use the string as the pattern:

```elixir
regex = "foo-bar" |> Regex.compile!()
"foo-bar" =~ regex
# => true
```

Matching a range of characters using square brackets `[]` to denote a _character class_. This will many any one character to the characters in the class. You can also specify a range of characters like `a-z`, as long as the start and end represent a contiguous range of codepoints.

```elixir
regex = "[a-z][AZ][0-9][!?]" |> Regex.compile!()
"aZ5!" =~ regex
# => true
"yA3?" =~ regex
# => true
```

_Shorthand character classes_ which make the pattern more concise. A small selection:

- `\d` short for `[0-9]` (any digit)
- `\w` short for `[A-Za-z0-9_]` (any 'word' character)
- `\s` short for `[ \t\r\n\f]` (any whitespace character)

> Note: when using `\` in a string pattern, you have to escape it, e.g. `"\\d"` to match a digit.

_Alternations_ use `|` as a special character to denote matching one _or_ another

```elixir
regex = "cat|bat" |> Regex.compile!()
"bat" =~ regex
# => true
"cat" =~ regex
# => true
```

_Quantifiers_ allow a for a repeating pattern in the regex. They affect the group preceeding the quantifier.

- `{N, M}` where `N` is the minimum number of repetitions, and `M` is the maximum
- `{0,}` or `*` matches zero-or-more repetitions
- `{1,}` or `+` matches zero-or-more repetitions
- `{,N}` match up to `N` repetitions

Round brackets `()` are used to denote _groups_ and _captures_. The group may also be _captured_ in some instances to be returned for use. In Elixir, these may be named or un-named. Groups function as a single unit, like when followed by _quantifiers_.

```elixir
regex =
  "(h)at"
  |> Regex.compile!()
  |> Regex.replace("hat", "\\1op")
# => "hop"

regex =
  "(?<letter_b>b)"
  |> Regex.compile!()
  |> Regex.scan("blueberry", capture: :all_names)
# => [["b"], ["b"]]
```

_Anchors_ are used to tie the regular expression to the beginning or end of the string to be matched:

- `^` anchors to the beginning of the string
- `$` anchors to the end of the string

The `~r` sigil provides a shortcut for `Regex.compile!/2`.

```elixir
"this is a test" =~ ~r/test/
# => true
```

> When using the sigil, you do not have to escape the `\` character, and the sigil accepts many delimiters to denote the start and end of the regex pattern.

Regular expressions are a complicated language unto themselves, but they are a useful tool for processing strings.
