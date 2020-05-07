# Role-playing game

## Story

In this exercise you're playing a role-playing game named "Wizard and Warriors," which allows you to play (unsurprisingly) as a wizard or a warrior.

The wizard and warrior have some common data and behavior:

- They have a number of hit points.
- They can attack, which reduces the number of hit points of the other character.
- If the number of hit points is less than or equal to zero, they are stunned and cannot do any more damage.

There are also some differences between the two character types.

|                    | Warriors          | Wizards             |
| ------------------ | ----------------- | ------------------- |
| Initial hit points | 30                | 20                  |
| Default attack     | 6                 | 3                   |
| Special attack     | 10 (potion drunk) | 12 (spell prepared) |

Drinking a potion (warrior) or preparing a spell (wizard) only makes the next attack a special attack. Subsequent attacks will be default attacks, unless a new potion is drunk or a new spell is prepared.

## Tasks

The story facilitates defining functions:

TODO

## Implementations

- [C#][implementation-csharp] (reference implementation)

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
[implementation-csharp]: ../../languages/csharp/exercises/concept/inheritance/.docs/instructions.md
