# Role-playing game

## Story

In this exercise you're playing a role-playing game named "Wizard and Warriors," which allows you to play as either a Wizard or a Warrior.

There are different rules for Warriors and Wizards to determine how much damage points they deal.

For a Warrior, these are the rules:

- Deal 6 points of damage if the attacked character is not vulnerable
- Deal 10 points of damage if the attacked character is vulnerable

For a Wizard, these are the rules:

- Deal 3 points of damage if the Wizard did not prepare a spell in advance
- Deal 12 points of damage if the Wizard did prepare a spell in advanced

In general, characters are never vulnerable. However, Wizard _are_ vulnerable if they haven't prepared a spell.

## Tasks

The story facilitates defining functions:

- Describe a character
- Make characters not vulnerable by default
- Allow Wizards to prepare a spell
- Make Wizards vulnerable when not having prepared a spell
- Calculate the damage points for a Wizard
- Calculate the damage points for a Warrior

## Implementations

- [C#: wizards-and-warriors][implementation-csharp] (reference implementation)

## Reference

- [`concepts/inheritance`][concepts-inheritance]
- [`concepts/constructors`][concepts-constructors]
- [`concepts/classes`][concepts-classes]
- [`types/integer`][types-integer]
- [`types/string`][types-string]

[concepts-classes]: ../concepts/classes.md
[concepts-constructors]: ../concepts/constructors.md
[concepts-inheritance]: ../concepts/inheritance.md
[types-integer]: ../types/integer.md
[types-string]: ../types/string.md
[implementation-csharp]: ../../languages/csharp/exercises/concept/wizards-and-warriors/.docs/instructions.md
