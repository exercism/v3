# C&#35; concept exercises

The concept exercises are based on this [list of concepts][reference-shared].

## Implemented exercises

These are the concept exercises that have currently been implemented, as well as the concepts they teach and their prerequisite concepts:

| exercise                                                            | concepts                                                             | prerequisites                                                                     |
| ------------------------------------------------------------------- | -------------------------------------------------------------------- | --------------------------------------------------------------------------------- |
| [`arrays`][concept-exercise-arrays]                                 | `arrays`<br/>`for-loops`<br/>`foreach-loops`                         | `booleans`<br/>`chars`<br/>`classes`<br/>`conditionals`                           |
| [`basics`][concept-exercise-basics]                                 | `basics`                                                             | -                                                                                 |
| [`booleans`][concept-exercise-booleans]                             | `booleans`                                                           | `basics`                                                                          |
| [`chars`][concept-exercise-chars]                                   | `chars`<br/>`Stringbuilder`                                          | `for-loops`<br/>`<br/>`strings`                                                   |
| [`classes`][concept-exercise-classes]                               | `classes`                                                            | `basics`<br/>`conditionals`<br/>`numbers`<br/>`strings`                           |
| [`constructors`][concept-exercise-constructors]                     | `constructors`                                                       | `classes`<br/>`conditionals`<br/>`numbers`<br/>`while-loops`                      |
| [`dates`][concept-exercise-datetimes]                               | `datetimes`                                                          | `classes`<br/>`numbers`<br/>`strings`                                             |
| [`dictionaries`][concept-exercise-dictionaries]                     | `dictionaries`                                                       | `foreach-loops`<br/>`generic-types`<br/>`indexers` <br/>`strings`                 |
| [`enums`][concept-exercise-enums]                                   | `enums`<br/>`pattern-matching-constants`                             | `conditionals`<br/>`strings`                                                      |
| [`flag-enums`][concept-exercise-flag-enums]                         | `bit-manipulation`<br/>`flag-enums`                                  | `attributes`</br>`enums`<br/>`integers`                                           |
| [`floating-point-numbers`][concept-exercise-floating-point-numbers] | `floating-point-numbers`<br/>`while-loops`                           | `conditionals`<br/>`numbers`                                                      |
| [`inheritance`][concept-exercise-inheritance]                       | `inheritance`                                                        | `booleans`<br/>`classes`<br/>`conditionals`<br/>`constructors`<br/>`strings`      |
| [`method-overloading`][concept-exercise-method-overloading]         | `method-overloading`<br/>`named-arguments`<br/>`optional-parameters` | `classes`<br/>`constructors`<br/>`enums`<br/>`properties`<br/>`strings`           |
| [`nullability`][concept-exercise-nullability]                       | `nullability`                                                        | `conditionals`<br/>`memory-allocation`<br/>`strings`                              |
| [`numbers`][concept-exercise-numbers]                               | `conditionals`<br/>`numbers`                                         | `basics`                                                                          |
| [`properties`][concept-exercise-properties]                         | `properties`                                                         | `classes`<br/>`enums`<br/>`exceptions`<br/>`floating-point-numbers`<br/>`numbers` |
| [`strings`][concept-exercise-strings]                               | `strings`                                                            | `basics`                                                                          |

It's only important that it's reasonably easy to _find_ the exercise. It's okay if the name isn't perfect. We **will** iterate on this.

## TODO

Thanks for wanting to contribute to the C# track's concept exercises! Contributions are very welcome!

To contribute, please find and work on one of the [new exercise issues][issues-new-exercise] or [improve exercise issues][issues-improve-exercise].

[reference-shared]: ../../reference/README.md
[reference]: ./reference.md
[concept-exercises]: ./concept/README.md
[concept-exercise-arrays]: ./arrays/.meta/design.md
[concept-exercise-basics]: ./basics/.meta/design.md
[concept-exercise-booleans]: ./booleans/.meta/design.md
[concept-exercise-chars]: ./chars/.meta/design.md
[concept-exercise-classes]: ./classes/.meta/design.md
[concept-exercise-constructors]: ./constructors/.meta/design.md
[concept-exercise-flag-enums]: ./flag-enums/.meta/design.md
[concept-exercise-datetimes]: ./datetimes/.meta/design.md
[concept-exercise-dictionaries]: ./dictionaries/.meta/design.md
[concept-exercise-enums]: ./enums/.meta/design.md
[concept-exercise-floating-point-numbers]: ./floating-point-numbers/.meta/design.md
[concept-exercise-inheritance]: ./inheritance/.meta/design.md
[concept-exercise-method-overloading]: ./method-overloading/.meta/design.md
[concept-exercise-nullability]: ./nullability/.meta/design.md
[concept-exercise-numbers]: ./numbers/.meta/design.md
[concept-exercise-properties]: ./properties/.meta/design.md
[concept-exercise-strings]: ./strings/.meta/design.md
[issues-new-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Fcsharp+label%3Atype%2Fnew-exercise+label%3Astatus%2Fhelp-wanted
[issues-improve-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Fcsharp+label%3Atype%2Fimprove-exercise+label%3Astatus%2Fhelp-wanted
