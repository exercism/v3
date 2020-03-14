### 1. Calculate the production rate per second

- Determining the success rate can be done through a [`cond` control flow statement][cond-control-flow].
- Elixir allows for operations to be applied to two different number types (such as an `integer` and a `float`) by converting to the type with the most precision. You may need to convert the number back to the intended type for the tests to pass.

### 2. Calculate the number of working items produced per second

- Converting from a `float` to an `integer` can be done by a number of guard functions in the Kernel module -- [Kernel guards][kernel-gurds].

[cond-control-flow]: https://elixir-lang.org/getting-started/case-cond-and-if.html#cond
[kernel-guards]: https://hexdocs.pm/elixir/Kernel.html#guards
