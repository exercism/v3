Elixir provides an elegant syntax working with binary data as we have seen with the `<<>>` special form provided for working with _bitstrings_.

## Binary data

The binary type is a specialization on the bitstring type. Where bitstrings could be of any length (any number of [bits][wiki-bit]), binaries are where the number of [bits][wiki-bit] can be evenly divided by 8. That is, when working with binaries, we often think of things in terms of [bytes][wiki-bytes] (8 [bits][wiki-bit]). A [byte][wiki-bytes] can represent integer numbers from `0` to `255`. It is common to work with [byte][wiki-byte] values in [hexadecimal][wiki-hexadecimal], `0x00 - 0xFF`.

Binaries literals are defined using the bitstring special form `<<>>` to define a literal. When defining a literal, we can use integer and string literals. Integer values greater than 255 will overflow and only the last 8 bits of the integer will be used. By default, the `::binary` modifier is applied to the value. We can concatenate binaries with the `<>/2` operator.

```elixir
<<255>> == <<0xFF>>
<<256>> == <<0>> # Overflowing bits are truncated
<<"Hello">> == <<72, 101, 108, 108, 111>>
```

A _null-byte_ is another name for `<<0>>`

## Strings are encoded binary data

Why does the bitstring special form `<<>>` allow string literals? It is because string are encoded binaries in Elixir! Strings are encoded in [UTF-8][wiki-utf8] format. That is, they are encoded in 8-bit (one-byte) chunks, which allows them to represent more than 255 characters. Therefore, in elixir, the length of a string may not be the same as its byte representation

```elixir
# This string has a string length of 5, but is made up of 7 bytes
string = "cześć"
String.length(string) != byte_size(string)

# Even emoji strings are encoded binaries
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

And like previous examples of pattern matching, we can use this in function signatures to match when selecting from multiple function clauses.

[wiki-bit]: https://en.wikipedia.org/wiki/Bit
[wiki-byte]: https://en.wikipedia.org/wiki/Byte
[wiki-hexadecimal]: https://en.wikipedia.org/wiki/Hexadecimal
[wiki-utf8]: https://en.wikipedia.org/wiki/UTF-8
