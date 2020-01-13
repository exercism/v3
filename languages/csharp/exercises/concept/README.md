# C&#35; concept exercises

The concept exercises are based on this [list of concepts][docs-concept-exercises].

## Implemented exercises

These are the concept exercises that have currently been implemented, as well as the concepts they teach and their prerequisite concepts:

| exercise                                                            | concepts                                                       | prerequisites                                                  |
| ------------------------------------------------------------------- | -------------------------------------------------------------- | -------------------------------------------------------------- |
| [`numbers`][concept-exercise-numbers]                               | `basic-numbers`<br/>`basic-type-conversion`<br/>`conditionals` | -                                                              |
| [`numbers-floating-point`][concept-exercise-numbers-floating-point] | `floating-point-numbers`<br/>`loops`                           | `basic-numbers`<br/>`basic-type-conversion`<br/>`conditionals` |
| [`strings`][concept-exercise-strings]                               | `basic-strings`                                                | -                                                              |
| [`enums`][concept-exercise-enums]                                   | `basic-enums`                                                  | `basic-strings`                                                |
| [`dates`][concept-exercise-dates]                                   | `basic-dates`<br/>`basic-time`<br/>`string-formatting`         | `basic-numbers`<br/>`basic-strings`                            |
| [`bitwise-operations`][concept-exercise-bitwise-operations]         | `bitwise-operations`<br/>`advanced-enums`                      | `basic-enums`                                                  |

**⚠ Note ⚠**: The idea here is to use a `concept` name for the exercise/folder, but perhaps use some sort of "progression", so they will naturally become a sort of path to traverse. In this example, the `numbers` exercise only teaches basic number usage, and the `numbers-floating-point` exercise builds on that and digs deeper into floating-point numbers.

It's only important that it's reasonably easy to _find_ the exercise. It's okay if the name isn't perfect. We **will** iterate on this.

## TODO

Thanks for wanting to contribute to the C# track's concept exercises! Contributions are very welcome!

To contribute, please find and work on one of the [new exercise issues][issues-new-exercise] or [improve exercise issues][issues-improve-exercise].

[docs-concept-exercises]: ../../reference/README.md
[reference]: ./reference.md
[concept-exercises]: ./concept
[concept-exercise-bitwise-operations]: ./concept/bitwise-operations
[concept-exercise-dates]: ./concept/dates
[concept-exercise-enums]: ./concept/enums
[concept-exercise-numbers-floating-point]: ./concept/numbers-floating-point
[concept-exercise-numbers]: ./concept/numbers
[concept-exercise-strings]: ./concept/strings
[issues-new-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Fcsharp+label%3Atype%2Fnew-exercise+label%3Astatus%2Fhelp-wanted
[issues-improve-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Fcsharp+label%3Atype%2Fimprove-exercise+label%3Astatus%2Fhelp-wanted
