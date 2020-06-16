# Classes

## The Concept

A _constructor_ is a special type of method that is invoked when creating a new class instance.

## What to cover

- **What is a constructor.** What is a constructor? When does it come into play?
- **How to define a constructor.** What is the syntax to define a parameterless or parameterized constructor?
- **How to pass arguments to constructors that have parameters.** What is the syntax to pass arguments to parameterized constructors?

## Exercises

### Remote control car race

This exercise models a race between remote control cars. Each remote control car has a speed and battery (data) that are passed as constructor arguments. Each race also takes a constructor parameter for the race distance. The reference implementation (C#) teaches:

- How to define a constructor.
- The difference between parameterless and parameterized constructor.
- Creating an instance of a class that requires arguments to be passed to its constructor.

#### Implementations

| Track | Exercise                              | Changes |
| ----- | ------------------------------------- | ------- |
| F#    | [constructors][implementation-csharp] | None    |

[concept-objects]: ./objects.md
[concept-state]: ./state.md
[implementation-csharp]: ../../languages/csharp/exercises/concept/constructors/.docs/introduction.md
