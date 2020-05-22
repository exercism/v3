# Inheritance

## The Concept

Defining [classes][concept-classes] as a hierarchy, with a specific class (Dog) inheriting the behavior and data of its more generic parent (Animal).

## What to cover

- **What is inheritance?** Why use inheritance?
- **How to use inheritance.** What is the syntax to inherit from a class? How to override methods, etc.
- **Are there limitations to inheritance?** Is there multi-inheritance or only single-inheritance?

## Exercises

### Role-playing game

This exercise models a role-playing game with wizards and warriors that can attack each other. The reference implementation (C#) teaches:

- Defining an abstract class
- Defining classes that inherit from an abstract class
- Overriding abstract methods
- Overriding virtual methods
- Calling the base class' constructor from a derived class
- Restrict access to inheriting classes using access modifiers

#### Implementations

| Track | Exercise                         | Changes |
| ----- | -------------------------------- | ------- |
| C#    | [classes][implementation-csharp] | None    |

[concept-classes]: ./classes.md
[implementation-csharp]: ../../languages/csharp/exercises/concept/inheritance/.docs/introduction.md
