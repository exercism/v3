Great job! Tuples are used commonly to group information informally. A common pattern through elixir is to group function return values with a status

```elixir
File.read("hello.txt")
# => {:ok, "World"}

File.read("invalid.txt")
# => {:error, :enoent}
```

Then when writing elixir functions, we can made use of an assertive style with pattern matching:

```elixir
def read_file() do
  {:ok, contents} = File.read("hello.txt")
  contents
end
```

It might occur to you that this function may crash if the file does not exist. Don't worry, in Elixir it is often said to **let it crash**, because in elixir applications, a supervising process will restart the application to a known-good state.

## Key-points

### Tuples

- Tuple literals are enclosed with curly braces, `{}`.
- Tuples may hold any data-type in contiguous memory.
- When manipulating a tuple, rather than mutating the existing tuple, a new one is created.
- The `Kernel` and `Tuple` modules have useful functions for working with tuples.

### Pattern Matching

- Pattern matching is explicitly performed using the match operator, `=/2`.

  - Matches succeed when the _shape_ of the data on the left side of the operator matches the right side
  - When matches succeed, variables on the left are bound to the values on the right
  - using an underscore, `_`, allows us to disregard the values in those places

  ```elixir
  {:ok, number, _} = {:ok, 5, [4.5, 6.3]}

  number
  # => 5 is bound to this variable
  ```

- Pattern match may also occur in a function clause head, so that only arguments that match the pattern will invoke the function.
- Variables can be bound in a function clause pattern match

  ```elixir
  defmodule Example do
    def named_function(:a = atom_variable) do
      {atom_variable, 1}
    end
  end

  Example.named_function(:a)
  # => {:a, 1}
  # The first function clause matches, so it is invoked

  Example.named_function(:b)
  # => ** (FunctionClauseError) no function clause matching in Example.named_function/1
  ```
