You can use atoms whenever you have a set of constants you want to express. Using atoms opens a type-safe way of interacting with constant values. An atom is defined by its name, follwing the atom format:

```elixir
# All atoms are preceeded with a ':' then follow with alphanumeric snake-cased characters
variable = :atom
```

_Atoms_ are internally represented by an integer in a lookup table, which are set automatically.  It is not possible to change this internal value.  It is generally considered to be an [anti-pattern][anti-pattern] to dynamically create atoms from user supplied input.

_Atoms_ are also often used to represent finite states. As many functions in Elixir's standard library return an atom to annotate the result:

```elixir
Enum.fetch([1], 0)
# => {:ok, 1}
Enum.fetch([1], 2)
# => :error
```

[anti-pattern]: https://en.wikipedia.org/wiki/Anti-pattern
