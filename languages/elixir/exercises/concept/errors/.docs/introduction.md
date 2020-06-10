Errors happen. In Elixir, while people often say to "let it crash", there are times when we need to rescue the function call to a known good state to fulfil a software contract. In some languages, errors are used as method of control flow, but in Elixir, this pattern is discouraged. We can often recognize functions that may rause an error just by their name: functions that raise errors are to have `!` at the end of their name. This is in comparison with functions that return `{:ok, value}` or `:error`. Look at these library examples:

```elixir
Map.fetch(%{a: 1}, :b)
# => :error
Map.fetch!(%{a: 1}, :b)
# => raises KeyError
```

## Try .. Rescue

Elixir provides a construct for rescuing from errors using `try .. rescue`

```elixir
try do                             #1
  raise "error"                    #2
rescue
  e in RuntimeError -> :error      #3
end
```

Let's examine this construct:

- **Line 1**, the block is declared with `try`
- **Line 2**, the function call which may error is placed here, in this case we are calling `raise/1`
- **Line 3**, in the `rescue` section, we pattern match on the _Module_ name of the error raised
  - on the left side of `->`:
    - `e` is matched to the error struct
    - `in` is a keyword in this occurrence
    - `RuntimeError` is the module to matched, but can match on any error module, or `_` all errors.
  - on the right side:
    - the instructions to be executed if the error matches

## To raise, or not to raise

As it's written in [Elixir's getting started guide][getting-started]:

> It’s up to your application to decide if an error while [performing an action] is exceptional or not. That’s why Elixir doesn’t impose exceptions on ... functions. Instead, it leaves it up to the developer to choose the best way to proceed.

[getting-started]: https://elixir-lang.org/getting-started/try-catch-and-rescue.html
