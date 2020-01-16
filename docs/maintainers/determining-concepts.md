# Choosing and defining Concepts

It is worth reading the start of the [Concept Exercises](../concept-exercises.md) file to familiarise yourself with Concept Exercises and why writing out concepts is important.

To know which Concept Exercises should exist, a list of the language's concepts should first be compiled. There are various resources to use when compiling this list: books, (official) documentation, your own experiences and the [concepts listed in this repository](../../reference/concepts/README.md).

As many languages use the same concepts (but possibly implemented differently), it can greatly help to look at what other languages have already done in this regard. See the [C# concepts](../../languages/csharp/reference/README.md) for an example.

It can also be helpful to group related concepts. For example, the Classes, Polymorphism and Inheritance concepts are all object-oriented concepts, which can then be grouped as "Object-oriented concepts".

Filling out the concepts will be iterative, it is hard to get this right immediately. While creating concepts exercises, you'll probably find missing concepts or perhaps want to split out concepts.

## Concept Exercises

Generally only **one new concept** should be introduced per concept exercise. 

In order to keep the exercises interesting, maintainers may choose to multiple _trivial_ Concepts in one exercise.
A _trivial_ Concept is one where we can reasonably assume that a programmer will quickly grasp it, because they will generally be learning a remapping of syntax for existing knowledge. Examples might be basic number usage (`+-/*`), or conditionals (`if/else`).

Every new concept introducted, regardless of how _trivial_ it seems, must have some attention give to it in the exercise's introductions. Each should focus on how that Concept is applied in the given language, with links to learn more on the topic. If there is nothing special about it, a language-agnostic link is helpful.

All Concepts covered in a Concept Exericse must either by taught by it, or must be marked as a prerequisite in the `config.json`.
