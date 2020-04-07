Elixir is a dynamically-typed language, meaning that the type of a variable is only checked at run-time. There is also no real concept of variable assignment.  Using the match `=` operator, we can bind a value to a variable name:

```elixir
variable = 10 # binding the value `10` to `variable`
```

Re-binding a variable's value is also done performed with the `=` operator. Once defined, a variable's type can change when re-bound.

```elixir
count = 1 # Bind an integer literal value of 1
count = 2 # Re-bind to the new value of 2

# Compiler does not error when binding a new type to the variable
count = false
```

Elixir is an [functional-programming language][functional-programming] and requires all functions to be defined in a _module_. The `defmodule` keyword is used to define a module. All modules are available to all other modules at run-time and do not require an _access modifier_ to make them visible to other parts of the program.  A _module_ is analogous to a _class_ in other programming languages.

```elixir
defmodule Calculator do
  # ...
end
```

_Functions_ can be defined inside of modules. Each function can have zero or more parameters. All parameters are dynamically typed, and the return type is not explicitly declared, it is the type of the value returned. An _access modifier_ can be specified for functions, making only desired functions available for use external to the module. In a function, the value of the last line is _implicitly returned_ to the calling function.

```elixir
defmodule Calculator do
  def add(x, y) do
    x + y
  end
end
```

Invoking a function is done by specifying its module- and function name and passing arguments for each of the functions's parameters. The module name may be omitted if the function is invoked inside of the module.

```elixir
sum = Calculator.add(1, 2)
```

Elixir supports one type of comment for inline documentation. Single line comments are preceded by `#`.

[functional-programming]: https://en.wikipedia.org/wiki/Functional_programming
