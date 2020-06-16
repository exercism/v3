# Classes

## The Concept

A `class` is the primary construct for keeping [state][concept-state], and usually represents one specific concept in your domain.
An `object` is a particular instance of a class.

For example, we may create a class called "Person" which acts a template of a human, with fields for `name` and `age`. We may then have multiple [objects][concept-objects] that are particular instances of that person, e.g. "Katrina" or "Jeremy".

## What to cover

- **What classes are.** What makes up a class? Data + methods? Is it a commonly-used data type?
- **How to define a class.** What is the syntax to define a class?
- **How to create class instances.** What is the syntax to create instances of a class.
- **How to access data or methods on a class instance.** How to access a class' data or methods?

## Exercises

### Remote control car

This exercise models a remote control car. Each remote control car has a speed and battery (data) and methods to drive the car and display the car's data. The reference implementation (C#) teaches:

- Defining a class.
- Creating an instance of a class.
- Updating state in a class by calling methods.
- Data hiding (encapsulation) using access modifiers.

#### Implementations

| Track | Exercise                         | Changes |
| ----- | -------------------------------- | ------- |
| F#    | [classes][implementation-csharp] | None    |

[concept-objects]: ./objects.md
[concept-state]: ./state.md
[implementation-csharp]: ../../languages/csharp/exercises/concept/classes/.docs/introduction.md
