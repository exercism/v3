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

## Additional Resources

See the following sections of the Erlang documentation for more information:

- [Expression Evaluation](http://erlang.org/doc/reference_manual/expressions.html#expression-evaluation)
- [Variables](http://erlang.org/doc/reference_manual/expressions.html#variables)
- [Function Calls](http://erlang.org/doc/reference_manual/expressions.html#function-calls)
- [Arithmetic Expressions](http://erlang.org/doc/reference_manual/expressions.html#arithmetic-expressions)
- [Function Declaration Syntax](http://erlang.org/doc/reference_manual/functions.html#function-declaration-syntax)
- [Module Syntax](http://erlang.org/doc/reference_manual/modules.html#module-syntax)
