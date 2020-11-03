# Function

## The Concept

In some languages, [functions][concept-functions] have their own type. This may or may not be a [generic][concept-generics].

## What to cover

- **How to define a function** How to define a function. How to add functionality to a function. How does scoping the function work?
- **How to use function parameters** How to define function parameters and how to use them.
- **How to return a value from a function** How to return a value from a function. Does it require a specific keyword?
- **How to call a function** How to call a function and pass arguments to it.

## Exercises

### Lasagna cooking

This exercise deals with cooking a lasagna dish in the oven. The reference implementation (F#) teaches:

- How to define a function with one or two parameters.
- How to use parameters in a function.
- How to return a value from a function.
- How to call a function.

#### Implementations

| Track  | Exercise                                          | Changes |
| ------ | ------------------------------------------------- | ------- |
| C#     | [lucians-luscious-lasagna][implementation-csharp] | None    |
| F#     | [basics][implementation-fsharp]                   | None    |
| Julia  | [functions-introduction][implementation-julia]    | None    |
| Ruby   | [basics][implementation-ruby]                     | None    |
| Elixir | [basics][implementation-elixir-lasagna]           | None    |

### Closure Maker

This exercise deals with creating set of functions which return functions that make use of closures.

- How to define anonymous (lambda) functions.
- How to return a function from a function.
- How to use a variable in a closure.

#### Implementations

| Track  | Exercise                                     | Changes |
| ------ | -------------------------------------------- | ------- |
| Elixir | [basics][implementation-elixir-closuremaker] | None    |

[implementation-csharp]: ../../languages/csharp/exercises/concept/lucians-luscious-lasagna/.docs/introduction.md
[implementation-fsharp]: ../../languages/fsharp/exercises/concept/basics/.docs/introduction.md
[implementation-julia]: ../../languages/julia/exercises/concept/lasagna/.docs/introduction.md
[implementation-ruby]: ../../languages/ruby/exercises/concept/lasagna/.docs/introduction.md
[implementation-elixir-lasagna]: ../../languages/elixir/exercises/concept/lasagna/.docs/introduction.md
[implementation-elixir-closuremaker]: ../../languages/elixir/exercises/concept/secrets/.docs/introduction.md
[concept-functions]: ../concepts/functions.md
[concept-generics]: ../concepts/generics.md
