# Design

## Goal

The goal of this exercise is to teach the student the Concept of Inheritance in C#.

## Learning objectives

- Know what inheritance is.
- Know how to inherit from a class.
- Know that all types inherit from `object`.
- Know what abstract and sealed classes are.
- Know what abstract and virtual methods are.
- Know how to override methods.
- Know about the `protected` visibility modifier.

## Out of scope

- Extending types through extension methods.

## Concepts

This Concepts Exercise's Concepts are:

- `inheritance`: know what inheritance is; know how to inherit from a class; know that all types inherit from `object`; know what abstract and sealed classes are; know what abstract and virtual methods are; know how to override methods; know about the `protected` visibility modifier.

## Prequisites

This Concept Exercise's prerequisites Concepts are:

- `classes`: know how to work with classes.
- `constructors`: know how to work with constructors.
- `strings`: know how to do basic string interpolation.
- `boolean`: know how to use boolean logic.
- `conditionals`: know how to do conditional logic.

## Representer

This exercise does not require any specific representation logic to be added to the [representer][representer].

## Analyzer

This exercise could benefit from the following rules added to the the [analyzer][analyzer]:

- Verify that the constructor of the `Character` class uses the `protected` modifier.
- Verify that the various fields used (hit points, spell prepared and potion drunk) use the `private` modifier.

[analyzer]: https://github.com/exercism/csharp-analyzer
[representer]: https://github.com/exercism/csharp-representer
