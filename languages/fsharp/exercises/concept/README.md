# F&#35; concept exercises

The Concept Exercises are based on this [list of concepts][reference-shared].

## Implemented exercises

These are the Concept Exercises that have currently been implemented, as well as the Concepts they teach and their prerequisite concepts:

| exercise                                                            | concepts                     | prerequisites                                                                          |
| ------------------------------------------------------------------- | ---------------------------- | -------------------------------------------------------------------------------------- |
| [`basics`][concept-exercise-basics]                                 | `basics`                     |
| [`booleans`][concept-exercise-booleans]                             | `booleans`                   | `basics`                                                                               |
| [`datetimes`][concept-exercise-datetimes]                           | `datetimes`                  | `numbers`<br/>`strings`<br/>`classes`                                                  |
| [`discriminated-unions`][concept-exercise-discriminated-unions]     | `discriminated-unions`       | `pattern-matching`                                                                     |
| [`floating-point-numbers`][concept-exercise-floating-point-numbers] | `floating-point-numbers`     | `numbers`<br/>`conditionals`                                                           |
| [`lists`][concept-exercise-lists]                                   | `lists`                      | `strings`<br/>`booleans`<br/>`pattern-matching`                                        |
| [`numbers`][concept-exercise-numbers]                               | `numbers`<br/>`conditionals` | `basics`                                                                               |
| [`pattern-matching`][concept-exercise-pattern-matching]             | `pattern-matching`           | `strings`<br/>`conditionals`                                                           |
| [`recursion`][concept-exercise-recursion]                           | `recursion`                  | `pattern-matching`<br/>`discriminated-unions`<br/>`lists`<br/>`higher-order-functions` |
| [`strings`][concept-exercise-strings]                               | `strings`                    | `basics`                                                                               |

## TODO

Thanks for wanting to contribute to the F# track's Concept Exercises! Contributions are very welcome!

To contribute, please find and work on one of the [new exercise issues][issues-new-exercise] or [improve exercise issues][issues-improve-exercise].

[reference-shared]: ../../reference/README.md
[reference]: ./reference.md
[concept-exercises]: ./concept/README.md
[concept-exercise-basics]: ./basics/.meta/design.md
[concept-exercise-booleans]: ./booleans/.meta/design.md
[concept-exercise-datetimes]: ./datetimes/.meta/design.md
[concept-exercise-lists]: ./lists/.meta/design.md
[concept-exercise-strings]: ./strings/.meta/design.md
[concept-exercise-numbers]: ./numbers/.meta/design.md
[concept-exercise-pattern-matching]: ./pattern-matching/.meta/design.md
[concept-exercise-recursion]: ./recursion/.meta/design.md
[concept-exercise-discriminated-unions]: ./discriminated-unions/.meta/design.md
[concept-exercise-floating-point-numbers]: ./floating-point-numbers/.meta/design.md
[issues-new-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Ffsharp+label%3Atype%2Fnew-exercise+label%3Astatus%2Fhelp-wanted
[issues-improve-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Ffsharp+label%3Atype%2Fimprove-exercise+label%3Astatus%2Fhelp-wanted
