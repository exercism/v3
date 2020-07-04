## General

- [Tuples](https://docs.python.org/3/tutorial/datastructures.html#tuples-and-sequences) are immutable.
  [Sequence Types](https://docs.python.org/3/library/stdtypes.html#typesseq) that can contain any data type.
- Tuples are [iterable](https://docs.python.org/3/glossary.html#term-iterable).
- Elements within tuples can be accessed via [bracket notation](https://stackoverflow.com/questions/30250282/whats-the-difference-between-the-square-bracket-and-dot-notations-in-python), using a zero-based index.
- Other [Common Sequence Operations](https://docs.python.org/3/library/stdtypes.html#common-sequence-operations) can also be used when working with tuples.

---

### 1. Extract coordinates.

- Remember: tuples allow access via _index_, using _brackets_. Indexes start at zero.

### 2. Format coordinates.

- The `tuple` constructor will take _any iterable_ as an argument, unpack it, and return a tuple.
- Strings are iterable in python.

### 3. Match coordinates.

- [in keyword](https://docs.python.org/3/reference/expressions.html#membership-test-operations) for testing membership.
- The `tuple()` constructor will make a tuple from _any iterable_.
- Could you re-use your convert_coordinate() function?

### 4. Combine matched records.

- [common sequence operations](https://docs.python.org/3/library/stdtypes.html#common-sequence-operations)
- Could you use the `compare_records()` function here?

### 5. "Clean up" & print out all records.

- Remember: tuples are _immutable_, but the contents can be accessed via _index_ using _bracket notation_.
- Tuples don't have to use parentheses unless there is _ambiguity_.
