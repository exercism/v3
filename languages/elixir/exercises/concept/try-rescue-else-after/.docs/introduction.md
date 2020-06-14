## Errors Expanded

Using `try..rescue` is a powerful construct for catching errors when they occur in programs so that function can return sensible values if it is required. The `try..rescue` construct also offers us two additional features we can make use of:

- the `else` block
  - When the try block succeeds, the result is matched to this block.

```elixir
try do
  :a
rescue
  _ -> :error
else
  :a -> :success
end
# => :success
```

- the `after` block
  - Whether the try block succeeds or raises, the code in the `after` block is always executed as long as the process is alive at that point.
  - The result of the `after` block is not returned to the calling scope.

```elixir
try do
  :a
rescue
  _ -> :error
else
  :a -> :success
after
  :some_action
end
# => :success
```

## Dynamic dispatch

Because of the way Elixir resolves function calls, it is possible to use an atom that

You are familiar with the form of most atoms:

```elixir
is_atom(:an_atom)
# => true
```

- A Module's name is also an atom:

```elixir
is_atom(ModuleName)
# => true
```

- we can call a function from the module referenced by the atom:

```elixir
defmodule MyModule do
  def message(), do: "My message"
end

atom = MyModule
atom.message()
# => "My message"
```
