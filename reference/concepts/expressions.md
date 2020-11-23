# Expressions

## The Concept

An expression is a combination of one or more [constants][concept-constants], [variables][concept-variables], [operators][concept-operators], and [functions][concept-functions] that the programming language interprets (according to its particular rules of precedence and of association) and computes to produce (["to return"][concept-return-values], in a stateful environment) another value. This process, as for mathematical expressions, is called [evaluation][concept-evaluation].

In simple settings, the resulting value is usually one of various primitive types, such as [numerical][type-number], [string][type-string], [logical][type-boolean], complex data type or many others.

For example, `2+3` is an [arithmetic][concept-arithmetic] and programming expression which evaluates to `5`. A variable is an expression because it denotes a value in memory, so `y+6` is an expression. An example of a relational expression is `4≠4`, which evaluates to `false`.

## What to cover

- **What is an expression?** Explain what an expression is and how it is different from a statement.

## Exercises

### Lasagna cooking

This exercise deals with cooking a lasagna dish in the oven. The reference implementation (Common Lisp) teaches:

- What an expression is.

#### Implementations

| Track       | Exercise                                       | Changes |
| ----------- | ---------------------------------------------- | ------- |
| Common Lisp | [socks-and-sexprs][implementation-common-lisp] |         |

[concept-arithmetic]: ./arithmetic.md
[concept-constants]: ./constants.md
[concept-evaluation]: ./evaluation.md
[concept-functions]: ./functions.md
[concept-operators]: ./operators.md
[concept-return-values]: ./return_values.md
[concept-variables]: ./variables.md
[type-boolean]: ../types/boolean.md
[type-number]: ../types/number.md
[type-string]: ../types/string.md
[implementation-common-lisp]: ../../languages/common-lisp/exercises/concept/socks-and-sexprs/.docs/introduction.md
