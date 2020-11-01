# Pac-Man

## Story

This story uses the classic arcade game _Pac-Man_ as a basis for teaching the [boolean type][types-boolean] / [boolean logic][concepts-boolean_logic] concept.

The theme is a derivative of Weng, Tseng, and Lee's (2010)[(1)][1] discussion about strategies for effective learning for logic by using game states.

The story uses several game state checks to practice [boolean expressions][concepts-boolean_logic] in the language constructs.

## Tasks

Game states explored:

- If _Pac-Man_ can eat a ghost.
- If _Pac-Man_ scores a point.
- If _Pac-Man_ loses the game.
- If _Pac-Man_ wins the game.

## Terminology

Terms used in the story:

- _Pac-Man_: the main character of the game, whom the player controls.
- Power pellet: an item which gives the ability to eat ghosts, it has a finite duration of action.
- Dot: an item which gives points, and upon eating them all, wins the stage.
- Ghost: the CPU opponent in the game, where if a ghost touches _Pac-Man_ without a power-pellet active, causes _Pac-Man_ to lose the stage.

## Implementations

- [Elixir: booleans][implementation-elixir] (reference implementation)
- [Python: bools][implementation-python]

## Reference

- [`concepts/boolean_logic`][concepts-boolean_logic]
- [`types/boolean`][types-boolean]

## References

[1][1]. Jui-Feng Weng, Shian-Shyong Tseng, Tsung-Ju Lee, Teaching Boolean Logic through Game Rule Tuning, IEEE Trans. Learning Technol. 3 (2010) 319–328. <https://doi.org/10.1109/TLT.2010.33>.

[1]: https://doi.org/10.1109/TLT.2010.33
[types-boolean]: ../types/boolean.md
[concepts-boolean_logic]: ../concepts/boolean_logic.md
[implementation-elixir]: ../../languages/elixir/exercises/concept/pacman-rules/.docs/instructions.md
[implementation-python]: ../../languages/python/exercises/concept/ghost-gobble-arcade-game/.docs/instructions.md
