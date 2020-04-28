Great work! [Anonymous functions][anon-fns] are commonly used in Elixir on their own, as return values, and as arguements in higher order functions such as `Enum.map/2`

```elixir
Enum.map([1, 2, 3], &(&1 + 1))
# => [2, 3, 4]
```

### Key points

- Functions are treated as first class citizens:
  - Can be assigned to variables.
  - Can be passed around like data as arguments and return values.
  - Can be created dynamically.
- [Anonymous functions][anon-fns] be created with the `fn` keyword:

  ```elixir
  varaible = fn param ->
    param + 1
  end
  ```

- [Anonymous functions][anon-fns] may be created with the [capture shorthand][capture]:

  ```elixir
  variable = &(&1 + 1)
  ```

- function references can be invoked with the `.` token

  ```elixir
  varaible = fn param ->
    param + 1
  end

  variable.(1)
  # => 2
  ```

- Bound varables from an outer scope can be used in an inner scope using the bound variable name.

[anon-fns]: https://elixir-lang.org/getting-started/basic-types.html#anonymous-functions
[capture]: https://dockyard.com/blog/2016/08/05/understand-capture-operator-in-elixir
