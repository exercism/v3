# C&#35; concept exercises

The concept exercises are based on this [list of concepts][reference-shared].

## Implemented exercises

These are the concept exercises that have currently been implemented, as well as the concepts they teach and their prerequisite concepts:

| exercise                                                            | concepts                                   | prerequisites                                                                     |
| ------------------------------------------------------------------- | ------------------------------------------ | --------------------------------------------------------------------------------- |
| [`arrays`][concept-exercise-arrays]                                 | `arrays`<br/>`foreach-loops`               | `classes`<br/>`chars`<br/>`booleans`<br/>`conditionals`<br/>`for-loops`           |
| [`basics`][concept-exercise-basics]                                 | `basics`                                   | -                                                                                 |
| [`classes`][concept-exercise-classes]                               | `classes`                                  | `basics`<br/>`accessibility`<br/>`classes`                                        |
| [`dates`][concept-exercise-datetimes]                               | `datetimes`                                | `numbers`<br/>`strings`<br/>`classes`                                             |
| [`enums`][concept-exercise-enums]                                   | `enums`<br/>`pattern-matching-constants`   | `strings`<br/>`conditionals`                                                      |
| [`flag-enums`][concept-exercise-flag-enums]                         | `flag-enums`<br/>`bit-manipulation`        | `enums`<br/>`attributes`</br>`integers`                                           |
| [`floating-point-numbers`][concept-exercise-floating-point-numbers] | `floating-point-numbers`<br/>`while-loops` | `numbers`<br/>`conditionals`                                                      |
| [`numbers`][concept-exercise-numbers]                               | `numbers`<br/>`conditionals`               | `basics`                                                                          |
| [`properties`][concept-exercise-properties]                         | `properties`                               | `numbers`<br/>`floating-point-numbers`<br/>`classes`<br/>`enums`<br/>`exceptions` |
| [`strings`][concept-exercise-strings]                               | `strings`                                  | `basics`                                                                          |

It's only important that it's reasonably easy to _find_ the exercise. It's okay if the name isn't perfect. We **will** iterate on this.

## TODO

Thanks for wanting to contribute to the C# track's concept exercises! Contributions are very welcome!

To contribute, please find and work on one of the [new exercise issues][issues-new-exercise] or [improve exercise issues][issues-improve-exercise].

[reference-shared]: ../../reference/README.md
[reference]: ./reference.md
[concept-exercises]: ./concept/README.md
[concept-exercise-arrays]: ./arrays/.meta/design.md
[concept-exercise-basics]: ./basics/.meta/design.md
[concept-exercise-classes]: ./classes/.meta/design.md
[concept-exercise-flag-enums]: ./flag-enums/.meta/design.md
[concept-exercise-datetimes]: ./datetimes/.meta/design.md
[concept-exercise-enums]: ./enums/.meta/design.md
[concept-exercise-floating-point-numbers]: ./floating-point-numbers/.meta/design.md
[concept-exercise-numbers]: ./numbers/.meta/design.md
[concept-exercise-properties]: ./properties/.meta/design.md
[concept-exercise-strings]: ./strings/.meta/design.md
[issues-new-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Fcsharp+label%3Atype%2Fnew-exercise+label%3Astatus%2Fhelp-wanted
[issues-improve-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Fcsharp+label%3Atype%2Fimprove-exercise+label%3Astatus%2Fhelp-wanted
