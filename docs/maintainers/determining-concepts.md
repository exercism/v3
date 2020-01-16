# Choosing and defining Concepts

It is worth reading the start of the [Concept Exercises](../concept-exercises.md) file to familiarise yourself with Concept Exercises and why writing out Concepts is important.

To determine which Concept Exercises should be written for your language, you should first compile a list of your language's Concepts. There are various resources to use when compiling this list: books, (official) documentation, your own experiences and the [concepts listed in this repository](../../reference/concepts/README.md).

As many languages use the same Concepts (but possibly implemented differently), it can greatly help to look at what other languages have already done in this regard. See the [C# concepts](../../languages/csharp/reference/README.md) for an example.

It can also be helpful to group related Concepts. For example, the Classes, Polymorphism and Inheritance Concepts are all object-oriented Concepts, which can then be grouped as "Object-oriented concepts".

Filling out the Concepts will be iterative, it is hard to get this right immediately. While creating concepts exercises, you'll probably find missing concepts or perhaps want to split out concepts.

When starting this process, we **strongly** recommend you seek help from others who have defined Concepts, by asking questions on the [#v3 channel on Slack](https://exercism-team.slack.com/archives/CR91YFNG3).

## Concept Exercises

Generally, only **one new Concept** should be introduced per Concept Exercise. 

In order to keep the exercises interesting, maintainers may choose to introduce multiple _trivial_ Concepts in one exercise.
A _trivial_ Concept is one where we can reasonably assume that most programmers will quickly grasp it, due to prior familiarity, albeit with differing syntax. Examples might be basic number usage (`+`, `-`, `*`, etc.), or conditionals (`if/else`).

Every new Concept introduced, regardless of how _trivial_ it may seem, must be explicitly mentioned in the exercise's introduction. Each mention should focus on how that Concept is applied in the given language, with links to learn more on the topic. If there is nothing special about it, a language-agnostic link is helpful.

All Concepts covered in a Concept Exercise must either be taught by it, or must be marked as a prerequisite in the `config.json`.
