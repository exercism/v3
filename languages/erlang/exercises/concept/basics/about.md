## Basics

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

## More on `=`

The `=` operator is used for much more than just simple variable assignment! It is also used for performing [pattern matching][pattern_matching] to bind values to variables. In the case above, `Count` is bound to the value `5`, then pattern matching is performed with `6` on the right side being matched to `5` on the left. `5` is not matched with `6`, so an error occurs. Pattern matching will be discussd in more detail in later exercises.

## Function Arity

In Erlang, two functions with the same name but different arities are treated as two _different_ functions. Erlang does not have a concept of overloaded functions and does not allow functions to take a variable number of arguments. In the following example, `add/2` and `add/3` are different functions and thus both need to be exported explicitly:

``` erlang
-export([add/2, add/3]).

add(X, Y) ->
  X + Y.

add(X, Y, Z) ->
  X + Y + Z.
```

## Commenting Conventions

A common practice is to start single-line comments with `%%` and to precede comments on the same line as code with `%`.
