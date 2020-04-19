# Miscellaneous Utilities

These are various helper utilities to use in managing the Python track.

## make-concept-exercise

The purpose of this utility is to create the (somewhat complex) structure of a concept exercise. At the moment it's not particularly powerful, but it will populate SLUG/.meta/config.json for you, and will create SLUG/.meta/design.md with minimal structure.

```
$ ./bin/make-concept-exercise bool-basic --forked-from csharp/bool-basic
create exercises/concept/bool-basic/.docs
create exercises/concept/bool-basic/.meta
touch exercises/concept/bool-basic/bool_basic.py
touch exercises/concept/bool-basic/bool_basic_test.py
touch exercises/concept/bool-basic/.docs/introduction.md
touch exercises/concept/bool-basic/.docs/instructions.md
touch exercises/concept/bool-basic/.docs/hints.md
touch exercises/concept/bool-basic/.docs/after.md
touch exercises/concept/bool-basic/.meta/design.md
touch exercises/concept/bool-basic/.meta/example.py
touch exercises/concept/bool-basic/.meta/config.json
```

Over time we can improve this with some templated contents for some of other files.
