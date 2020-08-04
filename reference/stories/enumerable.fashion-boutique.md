# Fashion Boutique

## Story

You are running an online fashion boutique. Black Friday is coming up, so you need to take stock of your inventory to make sure you're ready.

A single item in the inventory is represented by a map/dictionary, and the whole inventory is a list/array of such maps/dictionaries.

```elixir
%{
  name: "White Shirt",
  price: 40,
  quantity_by_size: %{s: 3, m: 7, l: 8, xl: 4}
}
```

## Tasks

- Sort items by price
- Find all items with missing prices
- Increment the item's quantity
- Calculate the item's total quantity

## Implementations

- [Elixir: enum][implementation-elixir] (reference implementation)
- [Elixir: list-comprehensions][extension-elixir] (adapted for further use in following exercise)

## Reference

- [`types/array`][types-array]
- [`types/dictionary`][types-dictionary]
- [`types/list`][types-list]
- [`types/map`][types-map]

[types-array]: ../types/array.md
[types-dictionary]: ../types/dictionary.md
[types-list]: ../types/list.md
[types-map]: ../types/map.md
[implementation-elixir]: ../../languages/elixir/exercises/concept/enum/.docs/instructions.md
[extension-elixir]: ../../languages/elixir/exercises/concept/list-comprehensions/.docs/instructions.md
