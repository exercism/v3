Functions are treated as first class citizens in Elixir. This means functions:

- Can be assigned to variables.
- Can be passed around like data as arguments and return values.
- Can be created dynamically.


Anonymous function start with the reserved word `fn`, the parameters are separated from the body of the function with the `->` token, and they are finished with an `end`. As with named functions, the last expression in the function is _implicitly returned_ to the calling function.

To invoke a function reference, you must use a `.` between the reference variable and the list of parameters:

```elixir
function_variable = fn param ->
  param + 1
end

function_variable.(1)
# => 2
```

You can even use short hand capture notation to make this more concise:

```elixir
variable = &(&1 + 1)

variable.(1)
# => 2
```

Which can be useful when are using other functions:

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

Closures in elixir are often used in this context. Variable scoping rules in elixir allow outer variables to be available to the inner scope if the variable hasn't been rebound in the inner scope:

```elixir
# Raises a compiler warning becuase x in the outer scope is unused.
def function(x) do
  fn x -> x end
end

# No warning is raised, x in the anonymous function refers to x from the outer scope.
def function(x) do
  fn a -> x.(a) end
end
```
