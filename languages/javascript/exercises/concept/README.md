# JavaScript concept exercises

The concept exercises in the JavaScript track are a work-in-progress and are based on this [list of concepts][reference-shared]. Important _types_ of concepts to target are things that only exist in object-oriented programming (for people coming from non-oop languages), functions as a first class citizen (for people coming from non-functional languages), and JavaScript specifics:

- prototype based inheritance
- event loop
- sameness
- destructuring
- duck typing

It is important to understand we _never_ explain a specific type or syntax as a concept, but teach the more "abstract" concept around it, using the type(s) or syntax(is).

A list of exercises that we _must_ have is compiled below, and is not at all a complete list:

- [x] [`/numbers`][concept-numbers]: `"numbers-basic"`, `"type-conversion"`
- [x] [`/strings`][concept-strings]: `"strings-basic"`, `"strings-concatenation"`
- [x] [`/promises`][concept-promises]: `"promises"`
- [ ] `"arrays-basic"`
- [ ] `"errors-basic"`
- [ ] `"functions"`: `"functions-basics"`, `"functions-defaults"`
- [ ] `"callbacks"`
- [ ] `"recursion"`

**⚠ Note ⚠**: The idea here is to use a `concept` name for the folder, but perhaps use some sort of "progression", so they will naturally become a sort of path to traverse. In this example, the `numbers` exercise only teaches basic number usage, and doesn't look into more advanced subjects. I would expect to see

- `numbers-advanced` for mathy usage that is non-basic,
- `numbers-irrational` showing how to do irrational / complex / whatever numbers,
- `numbers-precision` which would explore binary representation and floating points,
- `numbers-arbitrary-precision` which would explore `bigints` and/or how to do that with decimals.

It's only important that it's reasonably easy to _find_ the exercise. It's okay if the name isn't perfect. We **will** iterate on this.

## Concept interpretation

Here is how we have interpreted the following concept-keywords. This should be synced across tracks.

| concept           | interpretation                                                                                                                                                                                                                     |
| ----------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `numbers-basic`   | Know of the existence of the `number` type and (for this language) see that it's whole numbers, and floating points. Know of basic operators such as multiplication. Know where it's documented, or at least how to search for it. |
| `strings-basic`   | Know of the existence of the `string` type. Know of some basic functions (like looking up a character at a position, or slicing the string). Know where it's documented, or at least how to search for it.                         |
| `promises`        | Know of the `Promise` construct (`future` type in some other languages), chain-ability, and `.then`. Know where it's documented/how to search.                                                                                     |
| `arrays-basic`    | Know of the `Array` construct (`array` type in some other languages). Know of some basic functions.                                                                                                                                |
| `errors-basic`    | Know how to create an error, how throw it and how to catch it                                                                                                                                                                      |
| `functions`       | Know the different ways how to create a function, how to set default values and how to return a value and how to not.                                                                                                              |
| `callbacks`       | Know how to use a function as a first-class citizen, that is, pass it into a function as a value, and call it somewhere inside that function. Know that you can pass in values and retrieve out return values.                     |
| `recursion`       | Know how to call a function from itself                                                                                                                                                                                            |
| `type-conversion` | Know that there exist functions that can convert between "types" (objects of a prototype) and types (primitives).                                                                                                                  |

This also indicates that for example `strings-basic` does **not** include knowing that JavaScript strings aren't strings in the compsci sense as in, they are made up of UTF-16 codepoints, and therefore there are a lot of catches with this. `numbers-basic` does **not** talk about the `%` operator and how it's _remainder_ and not _modulo_, or the fact that binary operations on negative numbers are ... weird.

## TODO

Thanks for wanting to contribute to the JavaScript track's concept exercises! Contributions are very welcome!

To contribute, please find and work on one of the [new exercise issues][issues-new-exercise] or [improve exercise issues][issues-improve-exercise].

[reference-shared]: ../../reference/README.md
[concept-numbers]: ./numbers
[concept-strings]: ./strings
[concept-promises]: ./promises
[issues-new-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Fjavascript+label%3Atype%2Fnew-exercise+label%3Astatus%2Fhelp-wanted
[issues-improve-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Fjavascript+label%3Atype%2Fimprove-exercise+label%3Astatus%2Fhelp-wanted
