Maps in Elixir are the goto data structure for storing information in key-value pairs. In other languages, these might also be known as associative arrays (PHP), hashes (Perl 5, Raku), or dictionaries (Python).

Keys and values can be of any data type, but if the key is an atom we can make use of special access protocols. Maps do not guarantee the order of their entries when accessed or returned.


## Literal forms

An empty map is simply declared with `%{}`. If we want to add items to a map literal, we can use two forms:

```elixir
# If the key is an atom:
%{atom_key: 1}

# If the key is a different type:
%{1 => :atom_value}

# You can even mix these if the atom form comes second:
%{"first_form" => :a, atom_form: :b}
```

While there is no canonical format, choose a consistent way to represent the key-value literal pairs.

## Map module functions

Elixir provides many functions for working with maps to access/get/put/remove/drop values and keys.

```shell
iex> map = %{}
%{}
iex> map_a = Map.put(map, :a, 1)
%{a: 1}
iex> Map.get(map_a, :a)
1
```

Some functions require an anonymous or captured function to be passed into the function:

```shell
iex> increment = fn value -> value + 1 end
#Function<7.126501267/1 in :erl_eval.expr/5>
iex> empty_map = %{}
%{}
iex> a_map = Map.update(empty_map, :a, 0, increment)
%{a: 0}
iex> Map.update(a_map, :a, 0, increment)
%{a: 1}
```

## Maps as an `Enumerable` collection

Maps implement the _Enumerable_ protocol, which allow them to be transformed into a list or key-value tuples. Often maps are transformed into a correlating list, processed or transformed in some way, then returned.

```shell
iex> map = %{a: 1, b: 2, c: 3}
%{a: 1, b: 2, c: 3}
iex> list = Map.to_list(map)
[a: 1, b: 2, c: 3]
iex> hd(list)
{:a, 1}
iex> Enum.map(list, fn pair -> (pair |> elem(1)) + 1 end)
[2, 3, 4]
```

## Module attributes as constants

In elixir, we can define module attributes which can be used as constants in our functions.

```elixir
defmodule Example do

  # Defines the attribute as the value 1
  @constant_number 1
  # Overwrites the attribute with the value 2
  @constant_value 2

  @constant_list [1, 2, 3]

  def example_value() do
    # Returns the value 2
    @constant_value
  end

  def example_list() do
    # Returns the list [1, 2, 3]
    @constant_list
  end
end
```

When used this way, attributes can be any expression which can be evaluated at compilation time. After compilation, module attributes are not accessible since they are exanded during cimpilation, similar to defined macros in languages like C.

[hamt]: https://en.wikipedia.org/wiki/Hash_array_mapped_trie
