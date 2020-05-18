# Builtin types

TODO: SUMMARIZE BUILTIN TYPES IN PYTHON

## Common builtin primitives

- Numerical types

  - [`bool`][bool]
  - [`int`][int]
  - [`float`][float]
  - [`complex`][complex]

- Sequential container types

  - Arbitary byte sequences

    - [`bytearray`][bytearray]
    - [`bytes`][bytes]

  - Unicode text sequences

    - [`str`][str]

  - Homogenous sequences

    - [`list`][list]
    - [`range`][range]
    - [`tuple`][tuple]

  - Mapping unique keys to values

    - [`dict`][dict]

  - Unique, non-ordered values

    - [`set`][set]
    - [`frozenset`][frozenset]

## More rarely used internals

    - Access to the interior of other sequences

        - [`slice`][slice]
        - [`memoryview`][memoryview]

    - Object-orientation internals

        - [`object`][object]
        - [`property`][property]

    - Run-time introspection

        - [`type`][type]

[bool]: ./bool.md
[bytearray]: ./bytearray.md
[bytes]: ./bytes.md
[complex]: ./complex.md
[dict]: ./dict.md
[float]: ./float.md
[frozenset]: ./frozenset.md
[int]: ./int.md
[list]: ./list.md
[memoryview]: ./memoryview.md
[object]: ./object.md
[property]: ./property.md
[range]: ./range.md
[set]: ./set.md
[slice]: ./slice.md
[str]: ./str.md
[tuple]: ./tuple.md
[type]: ./type.md
