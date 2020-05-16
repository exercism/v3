# Functional programming

Functional programming is a programming paradigm, a way of writing code, where programs are built through the composition of functions. Functional programmers try to isolate the transformations of data from their definitions as much as possible and treat functions as first class citizens; meaning functions are treated similarly to other datatypes and can be stored in variables, passed as a parameter to, or returned by, other functions.

A pure function is a function which doesn't mutate state, assign variables or produce side-effects. This results in a deterministic function as the same input(s) will always lead to the same output regardless of state or other code in the program.

Proponents of functional programming state that the implementation of this paradigm often leads to more readable, modular and correct code, as it allows for easier testing and debugging by restricting side effects and dependencies.

These are functional programming concepts that are found in the majority of functional languages.
For each, this document will list a canonical reference that explains the concept in general terms.
The tracks should link to these references the first time the topic is referenced.
For example, the first time a person is expected to deal with immutability, the README should say "if you are unfamiliar with the concept of immutability, please read this explanation...."

Track-specific sub-concepts of these could be covered by specific track exercises.
Each section below aims to draw the distinction between general and language-specific concepts.

This document aims to cover all the core topics that a programmer with any functional programming background would know.

## Concepts

- [Anonymous functions.md](../concepts/anonymous_functions.md)
- [Expression oriented](../concepts/expression_oriented.md)
- [Function composition](../concepts/function_composition.md)
- [Higher order functions](../concepts/higher_order_functions.md)
- [Immutability](../concepts/immutability.md)
- [Nested functions](../concepts/nested_functions.md)
- [Partial application](../concepts/partial_application.md)
- [Pattern matching](../concepts/pattern_matching.md)
- [Pipelines](../concepts/pipelines.md)
- [Pure functions](../concepts/pure_functions.md)
- [Recursion](../concepts/recursion.md)
- [Repl](../concepts/repl.md)
- [Type inference](../concepts/type_inference.md)
