# Clock

## Concepts

### PEP 8 style guide

PEP 8 is the Python official style guide. Black is emerging as the defacto "pyfmt" tool: should we recommend it? (since the advent of `gofmt` and then `rustfmt`, I'm totally sold on opinionated auto-format tools: saves time and no more bikeshedding)

### Constants

Avoid "magic numbers", defining instead meaningfully named constants.

PEP 8 convention for constants: `UPPER_SNAKE_CASE` 

### Classes

### Methods

### Operator overload

How to overload the `+` and `-` operators using the `__add__` and `__sub__` special methods.

### Rich comparison methods

- `__eq__`

### String formatting

How to format strings:
- `%` operator
- `str.format`
- `f-strings`
