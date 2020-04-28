Great work, knowing how to effectivly use [maps][maps] is an import skill to use Elixir fluently.

## Key points

- [Maps][maps] are a data structure that holds key-value pairs.
- Keys can be of any type, but must be unique.
- Values can be of any type, they do not have to be unique.
- [Maps][maps] do not guarantee the order of their contents despite appearing to do so.
  - Their underlying implementation gives this misperception:
    - At small sized (<=32 entries), they are implemented as an ordered [Keyword list][keyword-list]
    - At larger sizes (>32 entries), they are implemented as a [hash array mapped trie][hamt] &#91;[1][stackoverflow]&#93;
- [Maps][maps] can be declared with a literal form:

  ```elixir
  # An empty map
  %{}

  # A map with the atom key :a associated to the integer value 1
  %{a: 1}

  # A map with the string key "a" associated to the float value 2.0
  %{"a" => 2.0}

  # A map with the map key %{} with the list value [1,2,3]
  %{%{}, [1,2,3]}
  ```

- The [Map module][map-module], included with the standard library, has many useful functions for using [maps][maps].

  ```elixir
  Map.get(%{a: 2}, :a)
  # => 2
  Map.get(%{a: 2}, :b)
  # => nil
  ```

- [Maps][maps] implement the Enumerable protocol, allowing use of [Enum module][enum] functions
- [Anonymous functions][anon-fn] or [captured function references][captured-fn] are often required as arguments for [Map][map-module] and [Enum module][enum] functions

  ```elixir
  # Increment the value by one, if it is not found, update it with 0
  Map.update(%{a: 1}, :a, 0, &(&1 + 1))
  # => %{a: 2}

  # Sorting by a specific sub-value
  list = [{"A", 4}, {"B", 3}, {"C", 2}, {"D", 1}]
  Enum.sort_by(list, &Kernel.elem(&1, 1))
  # => [{"D", 1}, {"C", 2}, {"B", 3}, {"A", 4}]
  ```

- [Module attributes][attr-as-const] may be used like [constants][attr-as-const] which are evaluated at compile-time.

  ```elixir
  defmodule Example do
    @standard_message "Hello, World!"

    def message(), do: @standard_message
  end
  ```

[hamt]: https://en.wikipedia.org/wiki/Hash_array_mapped_trie
[keyword-list]: https://elixir-lang.org/getting-started/keywords-and-maps.html#keyword-lists
[maps]: https://elixir-lang.org/getting-started/keywords-and-maps.html#maps
[integers]: https://elixir-lang.org/getting-started/basic-types.html
[strings]: https://elixir-lang.org/getting-started/basic-types.html#strings
[named-function]: https://elixir-lang.org/getting-started/modules-and-functions.html#named-functions
[default-arg]: https://elixir-lang.org/getting-started/modules-and-functions.html#default-arguments
[map-module]: https://hexdocs.pm/elixir/Map.html
[enum]: https://hexdocs.pm/elixir/Enumerable.html#content
[attr-as-const]: https://elixir-lang.org/getting-started/module-attributes.html#as-constants
[stackoverflow]: https://stackoverflow.com/a/40408469
[captured-fn]: https://elixir-lang.org/getting-started/modules-and-functions.html#function-capturing
[anon-fn]: https://elixir-lang.org/getting-started/basic-types.html#anonymous-functions
