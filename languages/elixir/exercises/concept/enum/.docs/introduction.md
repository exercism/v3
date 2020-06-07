`Enum` is a very useful module that provides a set of algorithms for working enumerables. It offers sorting, filtering, grouping, counting, searching, finding min/max values, and much more.

In Elixir, an enumerable is any data type that implements the `Enumerable` protocol. The most common of those are lists and maps.

```elixir
Enum.sort([4, 2, 3, 1])
# => [1, 2, 3, 4]
```

```elixir
Enum.empty?(%{})
# => true
```

Many `Enum` functions accept a function as a second argument.

```elixir
Enum.filter([1, 2, 3, 4, 5], fn x -> x > 3 end)
# => [4, 5]
```

The most common `Enum` functions and `map` and `reduce`.

`Enum.map/2` allows you to replace every element in an enumerable with another element. The second argument to `Enum.map/2` is a function that accepts the original element and returns its replacement.

```elixir
Enum.map([1, 2, 3, 4, 5], fn x ->
  x * x
end)
# => [1, 4, 9, 16, 25]
```

`Enum.reduce/2` allows you to _reduce_ the whole enumerable to a single value. To achieve this, a special variable called the _accumulator_ is used. The accumulator carries the intermediate state of the reduction between iterations.

The second argument to `Enum.reduce/2` is the initial value of the accumulator. The third argument is a function that accepts an element and an accumulator, and returns the new value for the accumulator.

```elixir
Enum.reduce([1, 2, 3, 4, 5], 100, fn x, acc ->
  acc - x
end)
# => 85
```

When using maps with `Enum` functions, the map gets automatically converted to a list of 2-tuples containing the key and the value.

```elixir
Enum.filter(%{"a" => 100, "b" => 99}, fn {_key, value} ->
  value < 100
end)
# => [{"b", 99}]
```

To transform it back to a map, use `Enum.into`.

```elixir
Enum.into([{"b", 99}], %{})
# => %{"b" => 99}
```
