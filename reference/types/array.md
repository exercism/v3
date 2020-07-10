# Array

## The Concept

A [collection][type-collection] data structure often consisting of a of elements of the same type, each identified by an array index. The simplest type of array is a linear (one-dimensional) array.

## What to cover

Tell a student what an array is in your language, how an array can be manipulated, and ensure they understand where to look for docs on arrays.

- **Explain what arrays are in your language?** Are the arrays fixed sized or not? Can an array contain elements of different types?
- **How to access an array element?** What is the syntax for accessing an array element? Where do indexes start from in your language?
- **How to create an array?** What is the syntax for creating a new array?
- **How to add or remove elements from an array?** What is the syntax for various manipulations of an array?

## Exercises

### Stack of cards

This exercise deals with manipulating a stack of playing cards. The reference implementation (JavaScript) teaches:

- Create an array
- Accessing an element from an array
- Changing an element from an array
- Deleting an element from an array
- Adding an element to an array
- Using the length of an array

#### Implementations

| Track      | Exercise                            | Changes |
| ---------- | ----------------------------------- | ------- |
| JavaScript | [arrays][implementation-javascript] | None    |

### Stack of cards 2

This exercise deals with analysis on a stack of playing cards. The reference implementation (JavaScript) teaches:

- Find the index of an item in an array
- Find the index of an item matching a predicate in an array
- Test a predicate against all items in an array
  - short-circuit on one true (`some`)
  - short-circuit on one false (`every`)

#### Implementations

| Track      | Exercise                              | Changes |
| ---------- | ------------------------------------- | ------- |
| JavaScript | [arrays][implementation-javascript-2] | None    |

### Garden bird count

This exercise has one working with garden bird counts. The reference implementation (C#) teaches:

- Define an array
- Access an element in an array
- Update an element in an array
- Use functions on an array

#### Implementations

| Track | Exercise                        | Changes                                    |
| ----- | ------------------------------- | ------------------------------------------ |
| C#    | [arrays][implementation-csharp] | None                                       |
| F#    | [arrays][implementation-fsharp] | Replace looping task with pattern matching |

[type-collection]: ./collection.md
[implementation-javascript]: ../../languages/javascript/exercises/concept/arrays/.docs/introduction.md
[implementation-javascript-2]: ../../languages/javascript/exercises/concept/array-analysis/.docs/introduction.md
