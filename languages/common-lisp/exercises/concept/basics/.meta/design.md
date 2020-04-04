This document describes how to implement the `basics` concept exercise for the
Common Lisp track.

## Goal

The goal for this exercise is to get the student familiar with the basics of
comments, s-expressions, cons, and symbols in Common Lisp.

That is to say:

- That all s-expressions are either atoms or cons pairs
- Comments begin with a variable number of `;` (or between `#|` and `|#`)
- Symbols are simple values that can refer to either other values or themselves

We will teach the basics of s-expressions via the use of `atom` and `consp`
along with `car` and `cdr` (with additional tests showing that `first` and
`rest` are good "modern" names).

## Learning objectives

- Know about atoms, conses and lists
- Know about the basic functions `car` and `cdr` (`first` and `rest`)
- Know that the `car` of an s-expression is called as a function when evaluated
- Know that the `cdr` are the function arguments
- Know that the value of the last s-expression in a `defun` is "automatically"
  returned
- Know how to write comments and some of the conventions regarding their usage
- Understand the basics of symbols, keywords, and quoting

## Out of scope

- Full exploration of cons cells and lists
- Manipulation and generation of s-expressions (`macros`)
- Any sequence functions or manipulations
- Assignment of values to symbols
- Keyword arguments or plists
- Any further details of `defun`
- Any discussion of value sameness

## Concepts

- `comments`
- `expressions`
- `cons`
- `symbols`

## Prerequisites

There are no prerequisites.

## Resources to refer to

N/A

### Hints

N/A

### After

Perhaps the symmetry between the structure of code and data could be reiterated
here and the possibility of writing code that writes code could be alluded to.

## Representer

- Currently, no extra logic needs to be added to the
  [representer](https://github.com/exercism/common-lisp-representer).

## Analyzer

- Currently, no extra logic needs to be added to the
  [analyzer](https://github.com/exercism/common-lisp-analyzer).

## Implementing

As this is meant to be a basics exercise, it should assume no prior knowledge of
any Lisp. The stub file should contain `defun`s for all of the functions that
need to be defined.

## Help

If you have any questions while implementing the exercise, please post the
questions as comments in this issue.
