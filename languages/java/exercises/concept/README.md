# Java Concept Exercises

The concept exercises in the Java track are a work-in-progress and are based on this [list of concepts][reference-shared]. Important _types_ of concepts to target are things that only exist in object-oriented programming (for people coming from non-oop languages) such as:

- Streams
- lambda functions
- overloading
- floats
- dates
- enums

It is important to understand we _never_ explain a specific type or syntax as a concept, but teach the more "abstract" concept around it, using the type(s) or syntax(is).

## Implemented exercises

These are the concept exercises that have currently been implemented, as well as the concepts they teach and their prerequisite concepts:

| implemented | exercise | concepts                                                            | prerequisites                       |
| ----------- | -------- | ------------------------------------------------------------------- | ----------------------------------- |
| [ ]         | numbers  | `numbers-basic`<br/>`type-conversion-numbers`<br/>`conditionals-if` | -                                   |
| [ ]         | strings  | `strings-basic`                                                     | -                                   |
| [ ]         | dates    | `dates-basic`<br/>`time-basic`                                      | `numbers-basic`<br/>`strings-basic` |

**⚠ Note ⚠**: The idea here is to use a `concept` name for the exercise/folder, but perhaps use some sort of "progression", so they will naturally become a sort of path to traverse. In this example, the `numbers` exercise only teaches basic number usage, and the `dates` exercise builds on that and digs deeper into how numbers are can be used.

It's only important that it's reasonably easy to _find_ the exercise. It's okay if the name isn't perfect. We **will** iterate on this.

## TODO

Thanks for wanting to contribute to the Java track's concept exercises! Contributions are very welcome!

To contribute, please find and work on one of the [new exercise issues][issues-new-exercise] or [improve exercise issues][issues-improve-exercise].

[reference-shared]: ../../../../reference/README.md
[issues-new-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Fjava+label%3Atype%2Fnew-exercise+label%3Astatus%2Fhelp-wanted
[issues-improve-exercise]: https://github.com/exercism/v3/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3Atrack%2Fjava+label%3Atype%2Fimprove-exercise+label%3Astatus%2Fhelp-wanted
