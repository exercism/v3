One of the key aspects of working with numbers in Elixir is the distinction between integers (numbers without a decimal separator) and floating-point numbers (numbers with one or more digits after a decimal separator).

The two most commonly used numeric types in Elixir are `integer` (a 32-bit integer) and `float` (a 64-bit floating-point number).

Numbers can be compared using the default comparison operators (`<`, `>`, `==`, etc.). These operators can be used in [`cond` control flow statements][cond-control-flow] to conditionally execute code.

To ensure a specific numeric type is returned you may need to convert `float` numbers to `integer` numbers using a [Kernel guard function].

[cond-control-flow]: https://elixir-lang.org/getting-started/case-cond-and-if.html#cond
[kernel-guards]: https://hexdocs.pm/elixir/Kernel.html#guards
