# Concept CI

The following tooling can be used to keep concept names consistent within a track.

## `concepts.csv`

This machine-readable file defines all concept slugs used in the track.
The first column contains the concept.
The second column **optionally** contains the category of the concept, e.g. some tracks differentiate between functional and object-oriented concepts.
The first row contains the headers, i.e. `concept,category`.

Concepts may contain annotations separated with a dot, `.`, from the concept name, e.g. `numbers.basic` and `numbers.advanced`.
These annotations can be used when a single concept has multiple exercises.
As a rule of thumb, annotations should only be used when multiple exercises refer to the same concept document, e.g. `string-formatting` might have its own document, while `strings.basic` and `strings.advanced` both refer to `strings.md`.

### Example

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

## CI checks

The following checks are currently available:

- Check if all concepts used by exercises in `config.json` are defined in `concepts.csv`
  - **sensitive to annotations** (e.g. `strings.basic` in concepts.csv will be treated as `strings.basic`. if `concepts.csv` only contains `strings`, it will error)
- Check if concept extraction docs only contains valid concepts, under the following assumptions:
  - The first list is ignored, as it is assumed to contain the example implementations
  - In the rest of the doc, all lists are assumed to have the following format: `- <concept>: <explanation>`. Entries that aren't lists are ignored
  - **sensitive to annotations**
- Check if all exercise directory names are valid concepts.
  - **sensitive to annotations**

### Workflow file

You can install the Concept CI checks for your track by creating a workflow in `.github/workflows/<language>-concept-ci.yml` with the following contents:

**COMING SOON (see #718)**

[julia-concepts-csv]: ../../languages/julia/reference/concepts.csv
[install-julia]: https://julialang.org/downloads/
