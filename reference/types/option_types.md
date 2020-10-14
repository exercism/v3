# Option types

An option type (sometimes known as a maybe type) is a polymorphic type that represents the encapsulation of an optional value; e.g., it is often used as the return type of [functions][ref-functions] which may or may not return a meaningful value when they are applied. It consists of a constructor which either is empty (often named `none`, or `Nothing`), or which encapsulates the original data type A (often written `Just A` or `some A`).

A distinct, but related concept outside of functional programming, which is popular in object-oriented programming, is called [nullable types][nullable-types] (often expressed as `A?`). The core difference between option types and nullable types is that option types support nesting (`Maybe (Maybe A)` ≠ `Maybe A`), while nullable types do not (`String??` = `String?`).

### Pizza Slice

You have a number of pizza slice shops in your town and you want to write a web app that will let you compare two different pizza configurations to let you know who will give you the bigger slice.
(Swift) teaches:

- the `nil` literal
- how to define optional types
- optional operators (`!`, `?`, `??`)
- optional binding
- optional patterns

#### Implementations

| Track | Exercise                          | Changes |
| ----- | --------------------------------- | ------- |
| Swift | [optionals][implementation-swift] | None    |

[implementation-swift]: ../../languages/swift/exercises/concept/optionals/.docs/instructions.md

---

[1] Option Types, Wikipedia.org. (2020). https://en.wikipedia.org/wiki/Option_type (accessed October 11, 2020).

[ref-functions]: ../concepts/functions.md
[nullable-types]: ./nullable.md
