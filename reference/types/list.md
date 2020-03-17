# List

A [collection][type-collection] data structure in which a sequence of items that are associated together in order, but are not physically associated together in a contiguous block of memory as they would be in an [array][type-array]. In a singly-linked list, for example, each item is a node that contains the value for that item and a reference to the next node in the array.

In many languages lists are _homogenous_ like arrays, and can contain only one type of item internally. In some languages, however, lists are _heterogenous_ and can contain multiple types of item.

## Exercises

### Language List

In this exercise, the student processes a list of laguages to learn on Exercism. The exercise aims to teach:

- adding an item to a list
- returning the first item from a list
- returning a list of all items but the first
- returning the number of items in a list.

#### Implementations

| Track   | Exercise                        | Changes                                                             |
| ------- | ------------------------------- | ------------------------------------------------------------------- |
| Clojure | [lists][implementation-clojure] | None                                                                |
| F#      | [lists][implementation-fsharp]  | Define non-empty list. Replace tail function with reverse function. |

[type-char]: ./char.md
[implementation-clojure]: ../../languages/clojure/exercises/concept/lists/.docs/introduction.md
[implementation-clojure]: ../../languages/fsharp/exercises/concept/lists/.docs/introduction.md
[type-array]: ./array.md
[type-collection]: ./collection.md
