# Go concept exercises

The concept exercises are based on this [list of concepts][docs-concept-exercises].

## Implemented exercises

These are the concept exercises that have currently been implemented, as well as the concepts they teach and their prerequisite concepts:

| exercise                              | concepts                     | prerequisites          |
| ------------------------------------- | ---------------------------- | ---------------------- |
| [`numbers`][concept-exercise-numbers] | `numbers`<br/>`conditionals` | `basics`               |
| [`slices`][concept-exercise-slices]   | `slices`                     | `arrays`<br/>`numbers` |
| [`strings`][concept-exercise-strings] | `strings`                    | `basics`               |

**⚠ Note ⚠**: The idea here is to use a `concept` name for the exercise/folder, but perhaps use some sort of "progression", so they will naturally become a sort of path to traverse.

It's only important that it's reasonably easy to _find_ the exercise. It's okay if the name isn't perfect. We **will** iterate on this.

## Contributing

Thanks for wanting to contribute to the Go track's concept exercises! Contributions are very welcome!

To contribute, please find and work on one of the [new exercise issues][issues-new-exercise] or [improve exercise issues][issues-improve-exercise].

[docs-concept-exercises]: ../../reference/README.md
[issues-new-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Fgo+label%3Atype%2Fnew-exercise+label%3Astatus%2Fhelp-wanted
[issues-improve-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Fgo+label%3Atype%2Fimprove-exercise+label%3Astatus%2Fhelp-wanted
[concept-exercise-strings]: ./strings/.meta/design.md
[concept-exercise-numbers]: ./numbers/.meta/design.md
[concept-exercise-slices]: ./slices/.meta/design.md
