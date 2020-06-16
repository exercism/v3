---
name: "[MAINTAINERS] Implement new Concept Exercise"
about: FOR MAINTAINERS ONLY - Use this template to create an issue to implement a
  Concept Exercise
title: "[<LANG>] Implement new Concept Exercise: <SLUG>"
labels: status/help-wanted, type/new-exercise
assignees: ''

---

[Use this issue template to describe how to implement a new concept exercises. Please fill in the issue template, and remove any sections wrapped in square brackets (such as this section!).]

This issue describes how to implement the `<SLUG>` concept exercise for the <LANG> track.

## Getting started

**Please please please read the docs before starting.** Posting PRs without reading these docs will be a lot more frustrating for you during the review cycle, and exhaust Exercism's maintainers' time. So, before diving into the implementation, please read up on the following documents:

- [The features of v3](https://github.com/exercism/v3/blob/master/docs/concept-exercises.md).
- [Rationale for v3](https://github.com/exercism/v3/blob/master/docs/rationale-for-v3.md).
- [What are concept exercise and how they are structured?](https://github.com/exercism/v3/blob/master/docs/features-of-v3.md)

Please also watch the following video:

- [The Anatomy of a Concept Exercise](https://www.youtube.com/watch?v=gkbBqd7hPrA).

## Goal

[A clear description of which concepts this exercise aims to teach.

For example:

The goal of this exercise is to teach the student how concept X is implemented in <LANG>. We will teach X through <LANG>'s Y type.]

## Learning objectives

[A list of the exercise's learning objectives.

For example:

- Know of the existence of type `Y`.
- Know of basic functions that work on `Y`.
- Know how to use `Y` in context `Z`.
- ...]

## Out of scope

[A list of things out of scope for this exercise.

For example:

- Memory and performance characteristics of `Y`.
- Advanced usage of `Y`.
- ...]

## Concepts

[List the concepts this exercise teaches. These concepts can be more fine-grained than their overarching concepts.

For example:

- `strings-basic`
- ...]

## Prerequisites

[List the prerequisite concepts that this exercise expects the student to already be familiar with.

For example:

- `numbers-basic`
- ...]

## Resources to refer to

[List suggestions to use as resources in the exercise's documentation file(s).

For example:

### Hints

- [How to use `Y` when doing `A`][http://test.com]
- ...

### After

- [Performance characteristics of using `Y`][http://test.com]
- ...]

## Representer

[If this exercise requires any modifications to the track's representer, list these changes here and the reason for these changes.

For example:

- Normalize ternary expressions to if statements, as we are not interested in the type of conditional that is used.
- ...

To help the implementer get started, add a link to the language's representer document.

If you don't know what representers are, feel free to leave this section empty.]

## Analyzer

[If this exercise requires an analyzer to be added for it, list the patterns the analyzer should detect here.

For example:

- If feature `K` is used, suggest using feature `L`.
- ...

To help the implementer get started, add a link to the language's analyzer document.

If you don't know what analyzers are, feel free to leave this section empty.]

## Implementing

[Please insert your track-specific implementation details here.]

## Help

If you have any questions while implementing the exercise, please post the questions as comments in this issue.
