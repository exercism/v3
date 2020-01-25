## Gigasecond

Concepts that are present for this exercise:

- [Duck Typing](https://github.com/exercism/v3/blob/master/reference/concepts/duck_typing.md)
  - In v2 the tests consisted of giving a Time object, but the base unit of a gigasecond is 1 second, and is the same as the `Time` objects base unit.  Both Integer (and Float) have the `+` method, and so appear to behave similarly, which we can take advantage of.
- [Return Value](https://github.com/exercism/v3/blob/master/reference/concepts/return_values.md)
  - Implicit return should happen in this exercise, as there is no early return requirement.
- [Arithmetic](https://github.com/exercism/v3/blob/master/reference/concepts/arithmetic.md)
  - Discussion of how if a Time is given, the rules of arithmetic does not apply here.  The return object may not be commutative.
- [Constants](https://github.com/exercism/v3/blob/master/reference/concepts/constants.md)
  - Unchanging things may be constants, and the important definition of `1_000_000_000` is worth having a constant defined, emulating the important values as constants on the `Math` module.
