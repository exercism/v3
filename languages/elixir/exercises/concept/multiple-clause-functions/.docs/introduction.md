Elixir allows programmers to write code that follows closely to the **Open-Close Principle**. That is, software should be open for extension, but closed for modification. This is an important principle, because as your software grows in size and complexity, changes to that software are more difficult to make.

## Multiple Function Clauses

Elixir facilitates following this principle by allowing functions to have multiple clauses, so instead of sprawling and hard-coded control-logic, pointed functions can be written to add/remove behavior easily.

Suppose we have a function `number/1` to test whether a number is a favorite.

```elixir
def number(n) do
  "Awesome, that's my favorite"
end
```

Now suppose we have been asked to only have the number `7` be the favorite, a person might write:

```elixir
def number(n) do
  cond do
    number == 7 -> "Awesome, that's my favorite"
    true -> "That's not my favorite"
  end
end
```

Instead, Elixir offers _multiple function clauses_ and _guards_ to write:

```elixir
def number(n) when n ==7 do
  "Awesome, that's my favorite"
end
def number(n) do
  "That's not my favorite"
end
```

At run-time, elixir will test, from top to bottom of the source file, which function clause to invoke.

## Guards

Guards are used to prevent elixir from invoking functions based on evaluation of the parameters by guard functions. Guards begin with the `when` keyword, followed by a boolean expression. Guard functions are special functions which:

- Must be pure and not mutate any global states.
- Must return strict `true` or `false` values.

A list of common guards are found in the [Elixir documentation][kernel-guards]

## Default arguments

Functions may declare default values for one or more arguments. When compiled, elixir creates a function definition for `number/0` (no arguments), and `number/1` (one argument).

```elixir
def number(n \\ 7) when n == 7, do: "Awesome, that's my favorite"
```

If more than one argument has default values, the default values will be applied to the function from left to right to fill in for missing parameters.

[kernel-guards]: https://hexdocs.pm/elixir/master/Kernel.html#guards
