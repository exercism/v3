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

### Naming Concept Exercises

Concept Exercises should be named after their story/theme, _not_ after its concept(s). The following rules apply to naming Concept Exercises:

1. Use lowercase.
1. Use [kebab-case][kebab-case].
1. Use the original name when forking an exercise without major changes.

Good examples:

- `tim-from-marketing`
- `lucians-luscious-lasagna`
- `calculator-conundrum`

Bad examples:

- `TIM-FROM-MARKETING`: should use lowercase (i.e. `tim-from-marketing`)
- `TimFromMarketing`: should use use kebab-case (i.e. `tim-from-marketing`)
- `floating-point-numbers`: should not use a concept

### Basics

When designing Concept Exercises, you'll find that the same basic Concepts will pop-up as prerequisites. As such, each track's first exercise should be an exercise that unlocks the `basics` concept, which aims to have the student become familiar with the basics of working in that language. The basics will differ between languages, but you can think of things like knowing how to define and call a function/method or how to define variables. For example, see [this C# exercise][csharp-lucians-luscious-lasagna].

To make the basics exercise the very first exercise, it must be the only exercise with no prerequisites. If an exercise has a prerequisite other than `basics`, you can omit the `basics` prerequisite.

### Naming Concepts

Concept Exercise concepts (as used in the `config.json` file) should be named according to the following rules:

1. Use lowercase.
1. Use [kebab-case][kebab-case].
1. Don't use a hierarchy.

Good examples:

- `classes`
- `bit-manipulation`
- `floating-point-numbers`

Bad examples:

- `CLASSES`: should use lowercase (i.e. `classes`)
- `BitManipulation`: should use use kebab-case (i.e. `anonymous-functions`)
- `numbers-floating-point`: should not use a hierarchical notation (use `floating-point-numbers` instead)
- `numbers.floating-point`: should not use a hierarchical notation (use `floating-point-numbers` instead)
- `numbers/floating-point`: should not use a hierarchical notation (use `floating-point-numbers` instead)

[kebab-case]: https://en.wiktionary.org/wiki/kebab_case
[csharp-lucians-luscious-lasagna]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/lucians-luscious-lasagna/.docs/introduction.md
