# JavaScript concept exercises

The concept exercises in the JavaScript track are a work-in-progress and are based on this [list of concepts][reference-shared]. Important _types_ of concepts to target are things that only exist in object-oriented programming (for people coming from non-oop languages), functions as a first class citizen (for people coming from non-functional languages), and JavaScript specifics:

- prototype based inheritance
- event loop
- sameness
- destructuring
- duck typing

It is important to understand we _never_ explain a specific type or syntax as a concept, but teach the more "abstract" concept around it, using the type(s) or syntax(is).

A list of exercises that we _must_ have is compiled below, and is not at all a complete list:

<!-- ordered lexographically -->

- [x] [`/array-loops`][concept-array-loops]: `"array-loops"`
- [x] [`/basics`][concept-basics]: `"basics"`
- [x] [`/booleans`][concept-booleans]: `"booleans"`
- [x] [`/closures`][concept-closures]: `"closures"`
- [ ] `"conditionals"`
- [x] [`elyses-analytic-enchantments`][concept-array-analysis]: `"array-analysis"`
- [x] [`elyses-destructured-enchantments`][concept-array-destructuring]: `"array-destructuring"`
- [x] [`elyses-enchantments`][concept-arrays]: `"arrays"`
- [x] [`elyses-transformative-enchantments`][concept-array-transformations]: `"array-transformations"`
- [ ] `"errors"`
- [x] [`fruit-picker`][concept-callbacks]: `"callbacks"`
- [x] [`/nullability`][concept-nullability]: `"nullability"`
- [x] [`/numbers`][concept-numbers]: `"numbers"`
- [x] [`/promises`][concept-promises]: `"promises"`
- [x] [`/recursion`][concept-recursion]: `"recursion"`
- [x] [`/strings`][concept-strings]: `"strings"`
- [ ] `"string-formatting"`
- [ ] `"variable-parameters"`

**⚠ Note ⚠**: The idea here is to use a `concept` name for the folder, but perhaps use some sort of "progression", so they will naturally become a sort of path to traverse. In this example, the `numbers` exercise only teaches basic number usage, and doesn't look into more advanced subjects. I would expect to see

- `numbers-advanced` for mathy usage that is non-basic,
- `numbers-irrational` showing how to do irrational / complex / whatever numbers,
- `numbers-precision` which would explore binary representation and floating points,
- `numbers-arbitrary-precision` which would explore `bigints` and/or how to do that with decimals.

It's only important that it's reasonably easy to _find_ the exercise. It's okay if the name isn't perfect. We **will** iterate on this.

## TODO

Thanks for wanting to contribute to the JavaScript track's concept exercises! Contributions are very welcome!

To contribute, please find and work on one of the [new exercise issues][issues-new-exercise] or [improve exercise issues][issues-improve-exercise].

[reference-shared]: ../../reference/README.md
[concept-basics]: ./basics
[concept-array-analysis]: ./elyses-analytic-enchantments
[concept-array-destructuring]: ./elyses-destructured-enchantments
[concept-array-loops]: ./array-loops
[concept-array-transformations]: ./elyses-transformative-enchantments
[concept-arrays]: ./elyses-enchantments
[concept-booleans]: ./booleans
[concept-callbacks]: ./fruit-picker
[concept-closures]: ./closures
[concept-nullability]: ./nullability
[concept-numbers]: ./numbers
[concept-promises]: ./promises
[concept-recursion]: ./recursion
[concept-strings]: ./strings
[issues-new-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Fjavascript+label%3Atype%2Fnew-exercise+label%3Astatus%2Fhelp-wanted
[issues-improve-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Fjavascript+label%3Atype%2Fimprove-exercise+label%3Astatus%2Fhelp-wanted
