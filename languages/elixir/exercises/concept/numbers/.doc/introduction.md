There are two different kinds of numbers in Elixir - integers and floats.

Integers are whole numbers.

```elixir
integer = 3
# => 3
```

Floats are numbers with one or more digits behind the decimal separator. They use the 64-bit double precision floating-point format.

```elixir
float = 3.45
# => 3.45
```

In the [`Integer`][module-integer-functions] and [`Float`][module-float-functions] modules you can find some useful functions for working with those types. Basic arithmetic operators are defined in the [`Kernel`][module-kernel-arithmetic-operators] module.

Integers and floats can be mixed together in a single arithmetic expression. Using a float in an expression ensures the result will be a float too.

```elixir
2 * 3
# => 6

2 * 3.0
# => 6.0
```

However, when doing division, the result will always be a float, even if only integers are used.

```elixir
6 / 2
# => 3.0
```

To convert a float to an integer, you can discard the decimal part with [`trunc/1`][trunc-1].

[module-integer-functions]: https://hexdocs.pm/elixir/Integer.html#functions
[module-float-functions]: https://hexdocs.pm/elixir/Float.html#functions
[module-kernel-arithmetic-operators]: https://hexdocs.pm/elixir/Kernel.html#+/2
[trunc-1]: https://hexdocs.pm/elixir/Kernel.html#trunc/1
