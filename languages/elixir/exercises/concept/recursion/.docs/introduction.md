Recursive functions are functions that call themselves.

A recursive function needs to have at least one _base case_ and at least one _recursive case_.

A _base case_ returns a value without calling the function again. A _recursive case_ calls the function again, modifying the input so that it will at some point match the base case.

Very often, each case is written in its own function clause.

```elixir
# base case
def count([]) do
  0
end

# recursive case
def count([_head | tail]) do
  1 + count(tail)
end
```

Recursive functions, if implemented incorrectly, might never stop executing. If this happens, you need to force stop your program.

This problem can be caused by:

- Forgetting to implement a base case.
- Not defining the base case as the first clause.
- Not modifying the argument properly when doing the recursive call, and thus never reaching the base case.
