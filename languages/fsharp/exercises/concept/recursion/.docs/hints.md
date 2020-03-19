### 1. Define the pizza types and options

- The `Pizza` type is a [recursive type][recursive-types], with the `ExtraSauce` and `ExtraToppings` cases "wrapping" a pizza.

### 2. Calculate the prize of pizza

- To handle the `Pizza` type being a [recursive type][recursive-types], define a [recursive function][recursive-functions].

### 3. Calculate the prize of an order

- The [list pattern][list-pattern] can be used to pattern match the list of pizzas.

[recursive-functions]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/recursive-functions-the-rec-keyword
[recursive-types]: https://fsharpforfunandprofit.com/posts/recursive-types-and-folds/#a-basic-recursive-type
[list-pattern]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching#list-pattern
