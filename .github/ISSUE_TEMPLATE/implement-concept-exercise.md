---
name: "[MAINTAINERS] Implement new concept exercise"
about: FOR MAINTAINERS ONLY - Use this template to create an issue to implement a concept exercise
title: "[<LANG>] Implement new concept exercise: <SLUG>"
labels: type/new-exercise, status/help-wanted
assignees: ""
---

This issue describes how to implement the `<SLUG>` concept exercise for the <LANG> track.

## Goal

A clear description of which concepts this exercise aims to teach.

For example:

The goal of this exercise is to teach the student how concept X is implemented in <LANG>. We will teach X through <LANG>'s Y type.

## Things to teach

A list of things the exercise aims to teach the student.

For example:

- Know of the existence of type `Y`.
- Know of some basic functions that work on `Y`.
- Know where `Y` is documented, or at least how to search for it.
- ...

## Things not to teach

A list of things that are outside the scope of this exercise and that the exercise should thus not teach.

For example:

- Memory and performance characteristics of `Y`.
- Advanced usage of `Y`.
- ...

## Concepts

List the concepts this exercise teaches. These concepts can be more fine-grained than their overarching concepts.

For example:

- `basic-strings`
- ...

## Prequisites

List the prerequisite concepts that this exercise expects the student to already be familiar with.

For example:

- `basic-numbers`
- ...

## Resources to refer to

List suggestions to use as resources in the exercise's documentation file(s).

For example:

### Hints

- [How to use `Y` when doing `A`][http://test.com]
- ...

### After

- [Performance characteristics of using `Y`][http://test.com]
- ...

## Representer

If this exercise requires any modifications to the track's representer, list these changes here and the reason for these changes.

For example:

- Normalize ternary expressions to if statements, as we are not interested in the type of conditional that is used.
- ...

To help the implementer get started, add a link to the language's representer document.

## Analyzer

If this exercise requires an analyzer to be added for it, list the patterns the analyzer should detect here.

For example:

- If feature `K` is used, suggest using feature `L`.
- ...

To help the implementer get started, add a link to the language's analyzer document

## Implementing

Please check the [how to implement a concept exercise guide][docs-how-to-implement-a-concept-exercise] for details on how to implement this exercise.

## Help

If you have any questions while implementing the exercise, please post the questions as comments in this issue.

[docs-how-to-implement-a-concept-exercise]: ./docs/how-to-implement-a-concept-exercise.md
