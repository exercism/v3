You can use [atoms][atom] whenever you have a set of constants to express. An atom is defined by its name, following the [atom][atom] format:

```elixir
# Atoms start with a ':' followed by alphanumeric snake-cased characters
variable = :an_atom
```

[_Atoms_][atom] are internally represented by an integer in a lookup table, which are set automatically. It is not possible to change this internal value. It is generally considered to be an [anti-pattern][anti-pattern] to dynamically create atoms from user supplied input.

[_Atoms_][atom] are also often used to represent finite states. Many functions in Elixir's standard library return an atom to annotate the result:

```elixir
Enum.fetch([1], 0)
# => {:ok, 1}
Enum.fetch([1], 2)
# => :error
```
[atom]: https://elixir-lang.org/getting-started/basic-types.html#atoms
[anti-pattern]: https://en.wikipedia.org/wiki/Anti-pattern
