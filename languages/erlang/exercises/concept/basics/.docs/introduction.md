To define a variable and bind a value to it, use the `=` operator:

```erlang
MyValue = 10.
```

Variable names must begin with an uppercase letter and are generally written in Pascal case.

To enforce immutabilty, once a variable is bound to a value, it cannot be updated:

```erlang
Count = 5,
Count = Count + 1.
% exception error: no match of right hand side value 6
```

Erlang is a functional language and requires all functions to be defined in a _module_. The `module` module attribute is used to define the name of the module.

```erlang
-module(calculator).
```

Each function can have zero or more parameters. The number of parameters in a function is known as its _arity_. Every function returns a value, which is the value of the last expression evaluated in the function body.

```erlang
add(X, Y) ->
  X + Y.
```

The function body ends with a `.`. When there are multiple expressions in a function, each expression before the last ends with a `,`.

```erlang
double(X) ->
  Two = 2,
  X * Two.
```

To invoke a function within its own module, simply pass values in as its parameters:

```erlang
add(1, 3).
```

To allow for a function to be invoked outside of its module, use the `export` module attribute to specify a list of exported functions containing each function's name and arity:

```erlang
-export([add/2, double/1]).
```

Invoking an exported function outside of its module is done by prepending the function invocation with the name of its module:

```erlang
calculator:add(1, 3).
```

Single line comments are preceded by `%`. Erlang does not support multi-line comments.
