## Goal

This exercise should focus on teaching about the special types of arguments to `defun`: `&key`, `&rest`, `&optional`.

## Learning objectives

- Know how to take optional arguments using `&optional`
- Know how to use `&rest` to take an arbitrary number of arguments
- Know how to take keyword arguments using `&key`
- Know how to set default values for optional / keyword arguments
- Be able to use `supplied-p` parameters with optional / keyword
  arguments

## Out of scope

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

The concept file for `named-parameters` can (and perhaps should) mention `&allow-other-keys`.

## Concepts

- `named-parameters`
- `rest-parameters`
- `default-parameters`

## Prerequisites

- `functions`
- `expressions`
- `integers`
- `arithmetic`
- `conditionals`

## Resources to refer to

- [Practical Common
  Lisp](http://www.gigamonkeys.com/book/functions.html)
- [Common Lisp Hyperspec](http://clhs.lisp.se/Body/m_defun.htm)

### Hints

- [Common Lisp
  Cookbook](https://lispcookbook.github.io/cl-cookbook/functions.html)
