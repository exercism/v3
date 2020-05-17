Strings in Elixir are delimited by double quotes, and they are encoded in UTF-8:

```elixir
iex> "Hi! ¡Hola! Cześć! Привет!"
"Hi! ¡Hola! Cześć! Привет!"
```

Strings can be concatenated using the `<>/2` operator:

```elixir
iex> "Welcome to" <> " " <> "New York"
"Welcome to New York"
```

Strings in Elixir support interpolation using the `#{}` syntax:

```elixir
iex> "6 * 7 = #{6 * 7}"
"6 * 7 = 42"
```

Elixir provides many functions for working with strings in the _String module_.

```elixir
iex> String.downcase("HELLO")
"hello"

iex> String.last("12345")
"5"
```

To put a newline character in a string, use the `\n` escape code:

```elixir
iex> IO.puts("1\n2\n3\n")
1
2
3

:ok
```

To comfortably work with texts with a lot of newlines, use the triple-double-quote heredoc syntax instead:

```elixir
iex> IO.puts("""
...> 1
...> 2
...> 3
...> """)
1
2
3

:ok
```
