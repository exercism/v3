# Keywords

Python has -- as of version 3.8 -- 35 distinct keywords that are reserved by the interpreter. You cannot use any of these as variable names, and each has specific syntactic forms in which it can appear. Unlike in other languages, some of Python's keywords can act as [_objects_][objects-general] (see [`True`][keyword-true], [`False`][keyword-false], and [`None`][keyword-none]) in a statement, while others can act as [_operators_][operators-general], for example [`and`][keyword-and] and [`or`][keyword-or]. A few of them also act as modifiers to other keywords, which is why they can appear in multiple sections below.

## Do-nothing placeholder

- [`pass`][keyword-pass]

## Keywords that appear as values

### Boolean values

- [`True`][keyword-true]
- [`False`][keyword-false]

### The null value

- [`None`][keyword-none]

## Conditional branching

- [`if`][keyword-if]
- [`elif`][keyword-elif]
- [`else`][keyword-else]

## Keywords that appear as operators

### Boolean logic

- [`not`][keyword-not]
- [`and`][keyword-and]
- [`or`][keyword-or]

### Identity testing

- [`is`][keyword-is]

### Membership testing

- [`in`][keyword-in]

## Looping

### Continuous loop on predicate

- [`while`][keyword-while]

### Enumeration of items

- [`for`][keyword-for]

### Control flow within loops

- [`break`][keyword-break]
- [`continue`][keyword-continue]

## Importing external functionality

- [`import`][keyword-import]
- [`from`][keyword-from]
- [`as`][keyword-as]

## Exception & errors

### Raise an arbitrary exception

- [`raise`][keyword-raise]

### Inline testing of invariants

- [`assert`][keyword-assert]

### Catching and reacting to exceptions

- [`try`][keyword-try]
- [`except`][keyword-except]
- [`else`][keyword-else]
- [`finally`][keyword-finally]

### Function / method creation

### Defining a named function

- [`def`][keyword-def]

### Defining an anonymous function

- [`lambda`][keyword-lambda]

### Exiting a function

- [`return`][keyword-return]
- [`yield`][keyword-yield]

## Namespace manipulation

- [`global`][keyword-global]
- [`nonlocal`][keyword-nonlocal]
- [`del`][keyword-del]

## Class creation

- [`class`][keyword-class]

## Working within Python's context managers

- [`with`][keyword-with]

## Asynchronous operations

- [`async`][keyword-async]
- [`await`][keyword-await]

[objects-general]: ../../../../../reference/concepts/objects.md
[operators-general]: ../../../../../reference/concepts/operators.md
[keyword-and]: ./and.md
[keyword-as]: ./as.md
[keyword-assert]: ./assert.md
[keyword-async]: ./async.md
[keyword-await]: ./await.md
[keyword-break]: ./break.md
[keyword-class]: ./class.md
[keyword-continue]: ./continue.md
[keyword-def]: ./def.md
[keyword-del]: ./del.md
[keyword-elif]: ./elif.md
[keyword-else]: ./else.md
[keyword-except]: ./except.md
[keyword-false]: ./false.md
[keyword-finally]: ./finally.md
[keyword-for]: ./for.md
[keyword-from]: ./from.md
[keyword-global]: ./global.md
[keyword-if]: ./if.md
[keyword-import]: ./import.md
[keyword-in]: ./in.md
[keyword-is]: ./is.md
[keyword-lambda]: ./lambda.md
[keyword-none]: ./none.md
[keyword-nonlocal]: ./nonlocal.md
[keyword-not]: ./not.md
[keyword-or]: ./or.md
[keyword-pass]: ./pass.md
[keyword-raise]: ./raise.md
[keyword-return]: ./return.md
[keyword-true]: ./true.md
[keyword-try]: ./try.md
[keyword-while]: ./while.md
[keyword-with]: ./with.md
[keyword-yield]: ./yield.md
