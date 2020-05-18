# Constants

## The Concept

A constant is a value that cannot be altered by the program during normal execution, i.e., the value is constant. When associated with an identifier, a constant is said to be "named," although the terms "constant" and "named constant" are often used interchangeably. This is contrasted with a [variable][concept-variables], which is an identifier with a value that can be changed during normal execution, i.e., the value is variable.

In some languages, or under some circumstances, only the memory address is constant, but not its contents, i.e. the _variable_ can not be re-[assigned][concept-assignment], but the _value_ can be [mutated][concept-mutation].

## What to cover

- **What values can be made constant?**: are there restrictions to what values can be made constant?
- **How to make a value a constant?**: what is the syntax to make a value constant?
- **How to use constants?**: how can one refer to a constant?

## Exercises

### Lasagna cooking

This exercise deals with cooking a lasagna dish in the oven. The reference implementation (Ruby) teaches:

- How to define a constant
- How to refer to a constant.

#### Implementations

| Track      | Exercise                            | Changes |
| ---------- | ----------------------------------- | ------- |
| JavaScript | [basics][implementation-javascript] | None    |
| Ruby       | [basics][implementation-ruby]       | None    |

[concept-assignment]: ./assignment.md
[concept-mutation]: ./mutation.md
[concept-variables]: ./variables.md
[implementation-javascript]: ../../languages/javascript/exercises/concept/basics/.docs/introduction.md
[implementation-ruby]: ../../languages/ruby/exercises/concept/basics/.docs/introduction.md
