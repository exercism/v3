# Ruby concept exercises

The concept exercises are based on this [list of concepts][reference-shared].

## Implemented exercises

These are the concept exercises that have currently been implemented, as well as the concepts they teach and their prerequisite concepts:

| exercise                                                            | concepts                             | prerequisites                                           |
| ------------------------------------------------------------------- | ------------------------------------ | ------------------------------------------------------- |
| [`strings`][concept-exercise-strings]                               | `strings`                            | -                                                       |
| [`lasagna`][concept-exercise-lasagna]                               | `basics`                             | -                                                       |
| [`numbers`][concept-exercise-numbers]                               | `numbers`<br/>`conditionals`         | `booleans`                                              |
| [`floating-point-numbers`][concept-exercise-floating-point-numbers] | `floating-point-numbers`<br/>`loops` | `numbers`<br/>`conditionals`                            |
| [`arrays`][concept-exercise-arrays]                                 | `arrays`<br/>`each-loops`            | `classes`<br/>`chars`<br/>`booleans`<br/>`conditionals` |
| [`booleans`][concept-exercise-booleans]                             | `booleans`                           | `instance-variables`                                    |
| [`instance-variables`][concept-exercise-instance-variables]         | `instance-variables`<br/>`nil`       | `basics`                                                |

**⚠ Note ⚠**: The idea here is to use a `concept` name for the exercise/folder, but perhaps use some sort of "progression", so they will naturally become a sort of path to traverse.

It's only important that it's reasonably easy to _find_ the exercise. It's okay if the name isn't perfect. We **will** iterate on this.

## TODO

Thanks for wanting to contribute to the Ruby track's concept exercises! Contributions are very welcome!

To contribute, please find and work on one of the [new exercise issues][issues-new-exercise] or [improve exercise issues][issues-improve-exercise].

[reference-shared]: ../../reference/README.md
[concept-exercise-arrays]: ./arrays/.meta/design.md
[concept-exercise-strings]: ./strings/.meta/design.md
[concept-exercise-lasagna]: ./lasagna/.meta/design.md
[concept-exercise-numbers]: ./numbers/.meta/design.md
[concept-exercise-booleans]: ./booleans/.meta/design.md
[concept-exercise-floating-point-numbers]: ./floating-point-numbers/.meta/design.md
[concept-exercise-instance-variables]: ./instance-variables/.meta/design.md
[issues-new-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Fruby+label%3Atype%2Fnew-exercise+label%3Astatus%2Fhelp-wanted
[issues-improve-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Fruby+label%3Atype%2Fimprove-exercise+label%3Astatus%2Fhelp-wanted
