# Boolean

## The Concept

A boolean represents one of two values: `true` or `false`. Logical operators (`!`, `&&`, `||`) are typically used with Boolean values and they return a Boolean value.

## What to cover

Tell a student what a boolean is in your language, what the boolean operators are, how boolean expressions can be build, and ensure they understand where to look for docs on booleans.

- **Describe how TRUE and FALSE values are represented in your language.** What is the syntax for representing boolean values? Are booleans objects or primitives?
- **Describe the operators used for representing AND, OR and NOT operations.** What is the syntax of the boolean operators?
- **How to build boolean expressions?** How to combine boolean values and boolean operators in order to construct logical expressions? What is the precedence of the operators?

## Exercises

### RPG game logic

This exercise lists all the available actions of the main character in a given situation. The reference implementation (Javascript) teaches:

- Boolean values.
- Boolean operators.
- Boolean operators precedence.

#### Implementations

| Track | Exercise                              | Changes |
| ----- | ------------------------------------- | ------- |
| C#    | [booleans][implementation-csharp]     | None    |
| F#    | [booleans][implementation-fsharp]     | None    |
| JS    | [booleans][implementation-javascript] | None    |

### Pac-man game logic

This exercise looks at various game events and determine if they have occured by looking at various game states. The reference implementation (Elixir) teaches:

- Boolean values.
- Boolean operators.
- Boolean operators precedence.

#### Implementations

| Track  | Exercise                          | Changes |
| ------ | --------------------------------- | ------- |
| Elixir | [booleans][implementation-elixir] | None    |

[implementation-csharp]: ../../languages/csharp/exercises/concept/booleans/.docs/introduction.md
[implementation-fsharp]: ../../languages/fsharp/exercises/concept/annalyns-infiltration/.docs/introduction.md
[implementation-javascript]: ../../languages/javascript/exercises/concept/booleans/.docs/introduction.md
[implementation-elixir]: ../../languages/elixir/exercises/concept/pacman-rules/.docs/introduction.md
