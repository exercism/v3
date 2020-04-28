Great work! [Anonymous functions][anon-fns] are commonly used in Elixir on their own, as return values, and as arguments in higher order functions such as `Enum.map/2`

```elixir
Enum.map([1, 2, 3], &(&1 + 1))
# => [2, 3, 4]
```

### Key points

- Functions are treated as first class citizens:
  - Can be assigned to variables.
  - Can be passed around like data as arguments and return values.
  - Can be created dynamically.
- [Anonymous functions][anon-fns] are created with the `fn` keyword:

  - they are invoked with the `.` token

  ```elixir
  function_variable = fn param ->
    param + 1
  end

  function_variable.(1)
  # => 2
  ```

  - they may be immediatly invoked on creation:

  ```elixir
  (fn x, y -> x + y end).(2, 3)
  # => 5
  ```

- [Anonymous functions][anon-fns] may be created with the [capture shorthand][capture]:

  ```elixir

  captured_variable = &(&1 + 1)

  captured_variable.(1)
  # => 2
  ```

- Named functions can also be [captured][capture]:

  - The initial `&` declares the start of the capture expression
  - `&1`, `&2`, and so on refer to the positional arguments of the anonymous function

  ```elixir
  # Instead of:
  fn tuple -> elem(tuple, 0) end

  # We can write:
  &elem(&1, 0)
  ```

  ```elixir
  # Instead of:
  fn a, b -> a <= b end

  # We can write using the function and the function's arity:
  &<=/2
  ```

- Bound varables from an outer scope can be used in an inner scope using the bound variable name to [create closures].

  ```elixir
  def return_closure(x) do
    fn -> x end
  end
  ```

[anon-fns]: https://elixir-lang.org/getting-started/basic-types.html#anonymous-functions
[capture]: https://dockyard.com/blog/2016/08/05/understand-capture-operator-in-elixir
[closures]: https://inquisitivedeveloper.com/lwm-elixir-45/
