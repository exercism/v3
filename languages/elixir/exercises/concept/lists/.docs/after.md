## Key Points

- Lists are a basic data type.
- Lists are implemented as a linked list.
- List can be written in literal form or head-tail notation.

  ```elixir
  # Literal Form
  []
  [1]
  [1, 2, 3]

  # Head-tail Notation
  []
  [1 | []]
  [1 | [2 | [3 | []]]]
  ```

- Head-tail notation can be used to append items to a list

```elixir
list = [2, 1]

[3, 2, 1] == [3 | list]
# => true
```

- There are several common functions for lists:
  - `hd/1` returns the _head_ of a list -- the _first_ item in a list.
  - `tl/1` returns the _tail_ of the list -- the list _minus_ the _first_ item.
  - `length/1` returns the number items in the list.
  - `in/2` returns a boolean value whether the item is an element in the list.
- They may contain any data-type and a mix of data-types.
