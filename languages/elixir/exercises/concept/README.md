# Elixir concept exercises

The concept exercises are based on this [list of concepts][docs-concept-exercises].

## Implemented exercises

These are the concept exercises that have currently been implemented, as well as the concepts they teach and their prerequisite concepts:

| exercise                    | concepts                                                   | prerequisites                                                  |
| --------------------------- | ---------------------------------------------------------- | -------------------------------------------------------------- |
| `anonymous-functions`       | `anonymous-functions`, `closures`, `bit-manipulation`      | `basics`                                                       |
| `basics`                    | `basics`                                                   | none                                                           |
| `booleans`                  | `booleans`                                                 | `basics`                                                       |
| `conditionals`              | `conditionals`, `atoms`                                    | `booleans`                                                     |
| `lists`                     | `lists`, `string-literals`                                 | `booleans`                                                     |
| `numbers`                   | `integers`, `floating-point-numbers`                       | `basics`                                                       |
| `tuples`                    | `tuples`, `pattern-matching`                               | `multiple-clause-functions`, `floating-point-numbers`, `atoms` |
| `strings`                   | `strings`                                                  | `lists`, `pattern-matching`                                    |
| `maps`                      | `maps`, `module-attributes-as-constants`                   | `lists`, `tuples`, `anonymous-functions`, `default-arguments`  |
| `multiple-clause-functions` | `multiple-clause-functions`, `guards`, `default-arguments` | `conditionals`, `string-literals`                              |

**⚠ Note ⚠**: The idea here is to use a `concept` name for the exercise/folder, but perhaps use some sort of "progression", so they will naturally become a sort of path to traverse.

It's only important that it's reasonably easy to _find_ the exercise. It's okay if the name isn't perfect. We **will** iterate on this.

## Contributing

Thanks for wanting to contribute to the Elixir track's concept exercises! Contributions are very welcome!

To contribute, please find and work on one of the [new exercise issues][issues-new-exercise] or [improve exercise issues][issues-improve-exercise].

[docs-concept-exercises]: ../../reference/README.md
[issues-new-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Felixir+label%3Atype%2Fnew-exercise+label%3Astatus%2Fhelp-wanted
[issues-improve-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Felixir+label%3Atype%2Fimprove-exercise+label%3Astatus%2Fhelp-wanted
