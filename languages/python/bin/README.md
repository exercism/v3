# Miscellaneous Utilities

These are various helper utilities to use in managing the Python track.

## make-concept-exercise

The purpose of this utility is to create the (somewhat complex) structure of a concept exercise. At the moment it's not particularly powerful, but it will populate SLUG/.meta/config.json for you, and will create SLUG/.meta/design.md with minimal structure.

```
$ ./bin/make-concept-exercise boolean-logic --forked-from csharp/boolean-logic
create exercises/concept/boolean-logic/.docs
create exercises/concept/boolean-logic/.meta
touch exercises/concept/boolean-logic/bool_basic.py
touch exercises/concept/boolean-logic/bool_basic_test.py
touch exercises/concept/boolean-logic/.docs/introduction.md
touch exercises/concept/boolean-logic/.docs/instructions.md
touch exercises/concept/boolean-logic/.docs/hints.md
touch exercises/concept/boolean-logic/.docs/after.md
touch exercises/concept/boolean-logic/.meta/design.md
touch exercises/concept/boolean-logic/.meta/example.py
touch exercises/concept/boolean-logic/.meta/config.json
```

Over time we can improve this with some templated contents for some of other files.
