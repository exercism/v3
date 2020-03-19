# F&#35; concept exercises

The Concept Exercises are based on this [list of concepts][reference-shared].

## Implemented exercises

These are the Concept Exercises that have currently been implemented, as well as the Concepts they teach and their prerequisite concepts:

| exercise                                  | concepts                                                            | prerequisites                                                                  |
| ----------------------------------------- | ------------------------------------------------------------------- | ------------------------------------------------------------------------------ |
| [`booleans`][concept-exercise-booleans]   | `booleans-basic`                                                    |                                                                                |
| [`lists`][concept-exercise-lists]         | `lists-basic`                                                       | `strings-basic`                                                                |
| [`dates`][concept-exercise-dates]         | `dates-basic`<br/>`time-basic`                                      | `numbers-basic`<br/>`strings-basic`                                            |
| [`strings`][concept-exercise-strings]     | `strings-basic`                                                     | -                                                                              |
| [`numbers`][concept-exercise-numbers]     | `numbers-basic`<br/>`type-conversion-numbers`<br/>`conditionals-if` | -                                                                              |
| [`recursion`][concept-exercise-recursion] | `recursion-basic`<br/>`pattern-matching-lists`                      | `pattern-matching-basic`<br/>`discriminated-unions-basic`<br/>`integers-basic` |

**⚠ Note ⚠**: The idea here is to use a `concept` name for the exercise/folder, but perhaps use some sort of "progression", so they will naturally become a sort of path to traverse. For example, the `numbers` exercise only teaches basic number usage, and the `numbers-floating-point` exercise builds on that and digs deeper into floating-point numbers.

It's only important that it's reasonably easy to _find_ the exercise. It's okay if the name isn't perfect. We **will** iterate on this.

## TODO

Thanks for wanting to contribute to the F# track's Concept Exercises! Contributions are very welcome!

To contribute, please find and work on one of the [new exercise issues][issues-new-exercise] or [improve exercise issues][issues-improve-exercise].

[reference-shared]: ../../reference/README.md
[reference]: ./reference.md
[concept-exercises]: ./concept/README.md
[concept-exercise-booleans]: ./booleans/.meta/design.md
[concept-exercise-dates]: ./dates/.meta/design.md
[concept-exercise-lists]: ./lists/.meta/design.md
[concept-exercise-strings]: ./strings/.meta/design.md
[concept-exercise-numbers]: ./numbers/.meta/design.md
[concept-exercise-recursion]: ./recursion/.meta/design.md
[issues-new-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Ffsharp+label%3Atype%2Fnew-exercise+label%3Astatus%2Fhelp-wanted
[issues-improve-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Ffsharp+label%3Atype%2Fimprove-exercise+label%3Astatus%2Fhelp-wanted
