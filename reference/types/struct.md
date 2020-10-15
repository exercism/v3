# Struct

Structs are types that allow for combining your code into logical units. In some languages, structs may just collections of heterogeneous data, similar to [records][type-record]. And in other languages structs may be more like [classes][type-class], allowing for the grouping of data and behavior in the form of [functions or methods][concept-functions] and may act as templates for the creation of instances of the struct.

## What to cover

- **What is a struct?** Why use structs?
- **How to define structs.** What is the syntax to define a struct?
- **How to create instances of a struct.** The syntax to create a new instance of a struct.
- **Access members of a struct.** How to use fields/methods on a struct instance or type.

## Exercises

### Windowing System

This exercise models the implementation of a simplified windowing system for an operating system:. The reference implementation (Swift) teaches:

- Define a Size struct
- Define a Position struct
- Define a Window class
- Add a method to resize windows
- Add a method to move windows
- Add methods to update the window text and display window information
- Create a new Window

#### Implementations

| Track | Exercise                                    | Changes |
| ----- | ------------------------------------------- | ------- |
| Swift | [structs-and-classes][implementation-swift] | None    |

[concept-functions]: ../concepts/functions.md
[type-product-type]: ./product_type.md
[type-record]: ./record.md
[type-class]: ./class.md
[implementation-swift]: ../../languages/swift/exercises/concept/structs-and-classes/.docs/introduction.md
