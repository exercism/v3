# Kitchen Calculator

## Story

The story facilitates the exploration of [tuples][type-tuple] in a language.

Basic premise that that often kitchen ingredients are measured in obscure ways and require different conversion depending on the amounts or the tools available on hand. Tuples are used to provide [duck typing][concept-duck-typing] to the numeric component of the measurement.

So this explores basic conversion of values using floating point numbers.

### Conversions

| From               | To      |
| ------------------ | ------- |
| 1 mL               | 1mL     |
| 1 cup              | 240mL   |
| 1 fluid ounce (US) | 29.57mL |
| 1 teaspoon         | 5 mL    |
| 1 tablespoon       | 15 mL   |

## Tasks

This should guide students how to use:

- Tuples
- Tuples as grouped information
- Getting values from a tuple by use of element index
- Getting values from a tuple using pattern matching
- Creating new tuples

## Implementations

- [Elixir][implementation-elixir]

[concept-duck-typing]: ../concepts/duck_typing.md
[type-tuple]: ../types/tuple.md
[implementation-elixir]: ../../languages/elixir/exercises/concept/tuples/.docs/instructions.md
