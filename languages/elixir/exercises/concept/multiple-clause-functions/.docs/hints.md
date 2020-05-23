## General

In Elixir's ['Getting Started Guide'][guide] there is a nice refresher about named functions.

## 1. Make the response when the guess matches the secret number

You can use a [guard][guard] to check if the numbers match.

## 2. Make the response when the guess is greater than the secret number

You can make use of a combination [multiple function clauses][multiple-fn-clauses] and [guards][guard].

## 3. Make the response when the guess is less than the secret number

You can make use of a combination [multiple function clauses][multiple-fn-clauses] and [guards][guard].

## 4. Make the responses when the guess is one more or one less than the secret number

You can make use of a combination [multiple function clauses][multiple-fn-clauses] and [guards][guard].

Pay attention to the order of the function clauses.

## 5. Make the response when there is no guess

You can make use of a [default argument][default-arg] for a guess. The default value does not have to be an integer.

[default-arg]: https://elixir-lang.org/getting-started/modules-and-functions.html#default-arguments
[guard]: https://hexdocs.pm/elixir/master/Kernel.html#guards
[guide]: https://elixir-lang.org/getting-started/modules-and-functions.html#named-functions
