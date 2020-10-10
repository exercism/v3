# Elixir concept exercises

The concept exercises are based on this [list of concepts][docs-concept-exercises].

## Implemented exercises

These are the concept exercises that have currently been implemented, as well as the concepts they teach and their prerequisite concepts:

| exercise                    | concepts                                                   | prerequisites                                                                                  |
| --------------------------- | ---------------------------------------------------------- | ---------------------------------------------------------------------------------------------- |
| `access-behaviour`          | `access-behaviour`                                         | `maps`, `strings`, `recursion`, `nil`                                                          |
| `agent`                     | `agent`                                                    | `processes`, `maps`, `structs`                                                                 |
| `anonymous-functions`       | `anonymous-functions`, `closures`, `bit-manipulation`      | `basics`                                                                                       |
| `basics`                    | `basics`                                                   | none                                                                                           |
| `binary-matching`           | `binaries`                                                 | `bitstrings`, `strings`, `pattern-matching`, `if`                                              |
| `bitstrings`                | `bitstrings`, `tail-call-recursion`                        | `charlists`, `recursion`, `pattern-matching`                                                   |
| `booleans`                  | `booleans`                                                 | `basics`                                                                                       |
| `charlists`                 | `charlists`, `case`                                        | `lists`, `recursion`, `pattern-matching`, `guards`                                             |
| `cond`                      | `cond`, `atoms`                                            | `booleans`                                                                                     |
| `enum`                      | `enum`                                                     | `lists`, `maps`, `atoms`, `tuples`, `nil`, `anonymous-functions`                               |
| `errors`                    | `errors`, `try-rescue`                                     | `anonymous-functions`, `pattern-matching`, `structs`                                           |
| `exceptions`                | `exceptions`                                               | `errors`, `access-behaviour`                                                                   |
| `keyword-lists`             | `keyword-lists`                                            | `lists`, `tuples`, `atoms`, `if`, `default-arguments`                                          |
| `lists`                     | `lists`, `string-literals`                                 | `booleans`                                                                                     |
| `list-comprehensions`       | `list-comprehensions`                                      | `keyword-lists`, `lists`, `maps`, `pattern-matching`                                           |
| `maps`                      | `maps`, `module-attributes-as-constants`                   | `lists`, `tuples`, `anonymous-functions`, `default-arguments`                                  |
| `multiple-clause-functions` | `multiple-clause-functions`, `guards`, `default-arguments` | `cond`, `string-literals`                                                                      |
| `nil`                       | `nil`, `if`                                                | `booleans`, `strings`                                                                          |
| `numbers`                   | `integers`, `floating-point-numbers`                       | `basics`                                                                                       |
| `processes`                 | `processes`, `pids`                                        | `atoms`, `recursion`, `pattern-matching`, `tuples`                                             |
| `regular-expressions`       | `regular-expressions`                                      | `strings`                                                                                      |
| `streams`                   | `streams`, `ranges`, `pipe-operator`                       | `enum`, `tuples`, `if`                                                                         |
| `strings`                   | `strings`                                                  | `lists`, `pattern-matching`                                                                    |
| `structs`                   | `structs`, `static-access-operator`                        | `maps`, `multiple-clause-functions`, `pattern-matching`, `strings`, `nil`, `default-arguments` |
| `recursion`                 | `recursion`                                                | `lists`, `pattern-matching`, `multiple-clause-functions`, `guards`                             |
| `try-rescue-else-after`     | `try-rescue-else-after`                                    | `io`, `try-rescue`                                                                             |
| `tuples`                    | `tuples`, `pattern-matching`                               | `multiple-clause-functions`, `floating-point-numbers`, `atoms`                                 |

**⚠ Note ⚠**: The idea here is to use a `concept` name for the exercise/folder, but perhaps use some sort of "progression", so they will naturally become a sort of path to traverse.

It's only important that it's reasonably easy to _find_ the exercise. It's okay if the name isn't perfect. We **will** iterate on this.

## Contributing

Thanks for wanting to contribute to the Elixir track's concept exercises! Contributions are very welcome!

To contribute, please find and work on one of the [new exercise issues][issues-new-exercise] or [improve exercise issues][issues-improve-exercise].

[docs-concept-exercises]: ../../reference/README.md
[issues-new-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Felixir+label%3Atype%2Fnew-exercise+label%3Astatus%2Fhelp-wanted
[issues-improve-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Felixir+label%3Atype%2Fimprove-exercise+label%3Astatus%2Fhelp-wanted
