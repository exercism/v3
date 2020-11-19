This issue describes how to implement the `lists` concept exercise for the Common Lisp track.

## Getting started

**Please please please read the docs before starting.** Posting PRs without reading these docs will be a lot more frustrating for you during the review cycle, and exhaust Exercism's maintainers' time. So, before diving into the implementation, please read up on the following documents:

- [The features of v3](https://github.com/exercism/v3/blob/master/docs/concept-exercises.md).
- [Rationale for v3](https://github.com/exercism/v3/blob/master/docs/rationale-for-v3.md).
- [What are concept exercise and how they are structured?](https://github.com/exercism/v3/blob/master/docs/features-of-v3.md)

Please also watch the following video:

- [The Anatomy of a Concept Exercise](https://www.youtube.com/watch?v=gkbBqd7hPrA).

## Learning objectives

- Be able to create a list using quote (`'`), `list`, and `cons`
- Know that lists are constructed from `cons` cells terminated by `NIL`
- Be able apply `car` / `first` and `cdr` / `rest` to get the parts of a list
- Know about `nth` and `second`, `third` ... `tenth` for accessing elements of a list
- Be able to use a list like a stack with `push` and `pop`
- Be able to combine lists with `append`
- Be able to find the length of a list with `length`

## Out of scope

- Any higher-order functions like `mapcar` or `count-if`
- Discussion of other sequence types in lisp
- Circular, association, or property lists
- The advanced `cXr` accessors like `cadr` or `cdadar`.
- `member` and sequence functions like `subseq` and `reverse`
- More advanced sequence functions like `search`, `substitute`, or `merge`

## Concepts

- `lists`

## Prerequisites

- `cons`
- `symbols`

## Resources to refer to

- [Practical Common Lisp](http://www.gigamonkeys.com/book/they-called-it-lisp-for-a-reason-list-processing.html)

### Hints

- [Tutorialspoint](https://www.tutorialspoint.com/lisp/lisp_lists.htm)

### After

- Show off some of the simpler `cXr` functions

## Representer

N/A

## Analyzer

- The analyser should be able to distinguish between the different forms of list construction (`'` vs `list` etc.)
- Be able to suggest alternatives like `car` or `first`

## Implementing

Remember that this is a basic exercise. It's important to have strong grasp on basic list manipulation and usage, but mastering all of the details is unimportant at this stage.
