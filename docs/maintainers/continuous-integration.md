# Continuous Integration

- [Markdown and JSON formatting](#markdown-and-json-formatting)
- [Concept CI](#concept-ci)
  - [`concepts.csv`](#conceptscsv)

  - [`concepts.csv`](#conceptscsv)
  - [CI checks](#ci-checks)

## Markdown and JSON formatting

The formatting of all Markdown and JSON files is automatically checked against the Markdown and JSON formatting used by [prettier][prettier].

## Concept CI

The following tooling can be used to keep concept names consistent within a track.

### `concepts.csv`

This machine-readable file defines all concept slugs used in the track.
The first column contains the concept.
The second column **optionally** contains the category of the concept, e.g. some tracks differentiate between functional and object-oriented concepts.
The first row contains the headers, i.e. `concept,category`.

Concepts may contain annotations separated with a dot, `.`, from the concept name, e.g. `numbers.basic` and `numbers.advanced`.
These annotations can be used when a single concept has multiple exercises.
As a rule of thumb, annotations should only be used when multiple exercises refer to the same concept document, e.g. `string-formatting` might have its own document, while `strings.basic` and `strings.advanced` both refer to `strings.md`.

#### Example

```csv
concept,category
encapsulation,object-oriented
classes,object-oriented
inheritance,object-oriented
anonymous-functions,functions
higher-order-functions,functions
local-functions,functions
numbers.basic,
numbers.advanced,
```

For a longer example, see the Julia track's [`concepts.csv`][julia-concepts-csv].

[julia-concepts-csv]: ../../languages/julia/reference/concepts.csv
[prettier]: https://prettier.io/
