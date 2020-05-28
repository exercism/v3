[Nil][nil-dictionary] is an English word meaning "nothing" or "zero". In Elixir, `nil` is a special value that means an _absence_ of a value.

```elixir
# I do not have a favorite color
favorite_color = nil
```

`nil` is an atom, but it is usually written as `nil`, not `:nil`.

```elixir
nil === :nil
# => true
```

You can check if a variable's value is `nil` using [`==`][kernel-equal], with pattern matching, or using the [`is_nil` guard][kernel-is-nil].

```elixir
def call(phone_number) do
  if phone_number == nil do
    :error
  end
end
```

```elixir
def call(phone_number) when is_nil(phone_number) do
  :error
end
```

```elixir
def call(nil) do
  :error
end
```

## `if`

Besides `cond`, Elixir also provides the macros [`if/2` and `unless/2`][getting-started-if-unless] which are useful when you need to check for only one condition.

[`if/2`][kernel-if] accepts a condition and two options. It returns the first option if the condition is _truthy_, and the second option if the condition is _falsy_. [`unless/2`][kernel-unless] does the opposite.

```elixir
age = 15

if age >= 16 do
  "You are allowed to drink beer in Germany."
else
  "No beer for you!"
end

# => "No beer for you!"
```

If the second option is not given, `nil` will be returned.

```elixir
age = 15

if age >= 16 do
  "You are allowed to drink beer in Germany."
end

# => nil
```

It is also possible to write an `if` expression on a single line. Note the comma after the condition.

```elixir
if age >= 16, do: "beer", else: "no beer"
```

`unless` with an `else` option should be avoided.

```elixir
# preferred
if age >= 16, do: "beer", else: "no beer"

# not preferred
unless age < 16, do: "no beer", else: "beer"
```

## _Truthy_ and _falsy_

A _truthy_ value is a value that is considered true when encountered in a boolean context, for example as a condition in an `if` expression. A _falsy_ value is the opposite.

In Elixir, there are only two _falsy_ values, `false` and `nil`.

```elixir
truthy? = fn x -> if x, do: "truthy", else: "falsy" end

truthy?.(true)
# => "truthy"
truthy?.(0)
# => "truthy"
truthy?.([])
# => "truthy"

truthy?.(false)
# => "falsy"
truthy?.(nil)
# => "falsy"
```

[getting-started-if-unless]: https://elixir-lang.org/getting-started/case-cond-and-if.html#if-and-unless
[nil-dictionary]: https://www.merriam-webster.com/dictionary/nil
[kernel-if]: https://hexdocs.pm/elixir/Kernel.html#if/2
[kernel-unless]: https://hexdocs.pm/elixir/Kernel.html#unless/2
[kernel-equal]: https://hexdocs.pm/elixir/Kernel.html#==/2
[kernel-is-nil]: https://hexdocs.pm/elixir/Kernel.html#is_nil/1
