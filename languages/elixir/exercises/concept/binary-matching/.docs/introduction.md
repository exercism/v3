Elixir provides an elegant syntax working with binary data as we have seen with the `<<>>` special form provided for working with _bitstrings_.

## Binary data

The binary type is a specialization on the bitstring type. Where bitstrings could be of any length (any number of bits), binaries are where the number of bits can be evenly divided by 8. That is, when working with binaries, we often think of things in terms of bytes (8 bits). A byte can represent integer numbers from `0 - 255`. It is common to work with byte values in hexadecimal, `0x00 - 0xFF`.

Binaries may use the bitstring special form `<<>>` to define a literal. When defining a literal, we can use integer literals (numbers greater than 255 will overflow and only the last 8 bits of the integer will be used), string literals. By default, the `::binary` modifier is applied to the value. We can concatenate binaries with the `<>/2` operator.

```elixir
# A binary representing a 'null byte'
<<0>> == <<0x00>>
<<255>> == <<0xFF>>
<<"Hello">> == <<72, 101, 108, 108, 111>>
```

## Strings are encoded binary data

Why does the bitstring special form `<<>>` allow string literals? It is because string are encoded binaries in Elixir! Strings are encoded in UTF-8 format. That is, they are encoded in 8-bit (one-byte) chunks, which allows them to represent more than 255 characters. Therefore, in elixir, the length of a string may not be the same as its byte representation

```elixir
# This string has a string length of 5, but is made up of 7 bytes
string = "hełło"
String.length(string) != byte_size(string)

# Even emoji characters are encoded binaries
"👍" == <<240, 159, 145, 141>>
```

## Pattern Matching on binary data

Pattern matching is even extended to binaries, and we can pattern match on a portion of binary data much like we could for a list.

```elixir
# Ignore the first 8 bytes, match and bind the remaining to `body
<<_::binary-size(8), body::binary>>
```

We can also do this for strings

```elixir
# match the first 5 bytes to `name`, match the string literal " the ", match remaining bytes to `species`
<<name::binary-size(5), " the ", species::binary>> = <<"Frank the Walrus">>
{name, species}
# => {"Frank", "Walrus"}
```

And like all pattern matching we can use this in function signatures to match when selecting from multiple function clauses.
