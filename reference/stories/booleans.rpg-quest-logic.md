# RPG Quest Logic

## Story

In this exercise, you'll be implementing the quest logic for a new RPG game a friend is developing. The game's main character is Annalyn, a brave girl with a fierce and loyal pet dog. Unfortunately, disaster strikes, as her best friend was kidnapped while searching for berries in the forest. Annalyn will try to find and free her best friend, optionally taking her dog with her on this quest.

After some time spent following her best friend's trail, she finds the camp in which her best friend is imprisoned. It turns out there are two kidnappers: a mighty knight and a cunning archer.

Having found the kidnappers, Annalyn considers which of the following actions she can engage in:

- _Fast attack_
- _Spy_
- _Signal prisoner_
- _Free prisoner_

## Tasks

These are example tasks that fit the story of Annalyn rescuing her best friend:

- Implement check for _Fast attack_: a fast attack can be made if the knight is sleeping, as it takes time for him to get his armor on, so he will be vulnerable.
- Implement check for _Spy_: the group can be spied upon if at least one of them is awake. Otherwise, spying is a waste of time.
- Implement check for _Signal prisoner_: the prisoner can be signalled using bird sounds if the prisoner is awake and the archer is sleeping, as archers are trained in bird signaling so they could intercept the message.
- Implement check for _Free prisoner_: Annalyn can try sneaking into the camp to free the prisoner, but this tactic will only work if the prisoner is awake and the other two characters are sleeping. If the prisoner is sleeping, they'll be startled by Annalyn's sudden appearance and will awaken the other two characters. The prisoner can also be freed if the archer is sleeping and Annalyn has her pet dog with her, as the knight will be scared by the dog and will withdraw, and the archer can't equip his bow fast enough to prevent the prisoner from being freed.

## Terminology

These are recommendations, not rules, for recurring terminology in the instructions (including stub commentary)

- The main character is **Annalyn** and her pronouns are **she/her**. If you need to indicate the character, use the name or refer to her by her pronoun(s).
- Instead of `boolean` (or equivalent data type), use **can {action}** or **can perform {action}**
- The prisoner is her best friend, and their pronouns are **they/their**.
- Do _not_ refer to **Annalyn**, the **prisoner**, the **archer**, the **knight** or Annalyn's **pet dog** by pronoun, as the meaning will most likely be ambiguous; instead use the indicators as emphasized.

## Implementations

- [F#: booleans][implementation-fsharp]
- [JavaScript: booleans][implementation-javascript] (reference implementation)
- [Julia: boolean-logic][implementation-julia]

## Name suggestions

- [Annalyn's Analytical Avenge Attempt][implementation-julia]

## Related

- [`types/boolean`][types-boolean]

[types-boolean]: ../types/boolean.md
[javascript-concept-booleans]: ../../languages/javascript/exercises/concept/booleans
[implementation-fsharp]: ../../languages/fsharp/exercises/concept/booleans/.docs/instructions.md
[implementation-javascript]: ../../languages/javascript/exercises/concept/booleans/.docs/instructions.md
[implementation-julia]: ../../languages/julia/exercises/concept/boolean-logic/.docs/instructions.md
