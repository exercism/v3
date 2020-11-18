## General

- Try to split a problem into a base case and a recursive case. For example, let's say you want to count how many cookies are there in the cookie jar with a recursive approach. A base case is an empty jar - it has zero cookies. If the jar is not empty, then the number of cookies in the jar is equal to one cookie plus the number of cookies in the jar after removing one cookie.

## 1. Define the pizza types and options

- The `Pizza` type is a [recursive type][recursive-types], with the `ExtraSauce` and `ExtraToppings` cases "wrapping" a pizza.

## 2. Calculate the price of pizza

- To handle the `Pizza` type being a [recursive type][recursive-types], define a [recursive function][recursive-functions].

## 3. Calculate the price of an order

- If the test fails with a Stack overflow, please use [tail-recursion][tail-recursion] to recursively process the list.

[recursive-functions]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/recursive-functions-the-rec-keyword
[recursive-types]: https://fsharpforfunandprofit.com/posts/recursive-types-and-folds/#a-basic-recursive-type
[tail-recursion]: https://cyanbyfuchsia.wordpress.com/2014/02/12/recursion-and-tail-recursion-in-f/
