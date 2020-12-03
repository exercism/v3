## Goal

This exercise should focus on teaching `defun` The student should be comfortable defining top-level functions. They should also be introduced to the usage of docstrings.

## Learning objectives

- Know how to define a global top-level function using `defun`
- Know how to annotate a `defun` with a docstring
- Be able to define a function taking parameters

## Out of scope

- Optional parameters (`&optional` including setting default value and using `supplied-p` parameter)
- Rest parameters (`&rest`)
- Keyword arguments (`&key` including setting default value and using `supplied-p` parameter)
- Anonymous functions using `lambda`
- Local functions using `flet`, `labels`, etc.
- Recursion / self-referential functions
- The somewhat arcane `&aux` keyword
- The `&allow-other-keys` keyword
- Multiple value returns with `values`
- Early returns with `return-from`
- Methods / generic functions
- Higher-order functions
- Macros

## Concepts

- `functions`

## Prerequisites

- `expressions`
- `integers`
- `arithmetic`

## Resources to refer to

- [Practical Common
  Lisp](http://www.gigamonkeys.com/book/functions.html)
- [Common Lisp Hyperspec](http://clhs.lisp.se/Body/m_defun.htm)

### Hints

- [Common Lisp
  Cookbook](https://lispcookbook.github.io/cl-cookbook/functions.html)

## Implementing

Contrary to most exercises, the stub file for this exercise should not
contain any `defun`'s. Comments should be used to indicate which
functions the student needs to implement.
