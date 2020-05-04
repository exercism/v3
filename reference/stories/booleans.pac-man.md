# Pac-man

This story uses the classic arcade game _Pac-Man_ as a basis for teaching the [boolean type][boolean-type] / [boolean logic][boolean-logic] concept.

The theme is a derivative of Weng, Tseng, and Lee's (2010)[(1)][1] discussion about strategies for effective learning for logic by using game states.

The story uses several game state checks to practice [boolean expressions][boolean-logic] in the language constructs.

Game states explored:

- If _Pac-Man_ can eat a ghost.
- If _Pac-Man_ scores a point.
- If _Pac-Man_ loses the game.
- If _Pac-Man_ wins the game.

Terms used in the story:

- _Pac-Man_: the main character of the game, whom the player controls.
- Power pellet: an item which gives the ability to eat ghosts, it has a finite duration of action.
- Dot: an item which gives points, and upon eating them all, wins the stage.
- Ghost: the CPU opponent in the game, where if a ghost touches _Pac-Man_ without a power-pellet active, causes _Pac-Man_ to lose the stage.

## References

[1][1]. Jui-Feng Weng, Shian-Shyong Tseng, Tsung-Ju Lee, Teaching Boolean Logic through Game Rule Tuning, IEEE Trans. Learning Technol. 3 (2010) 319–328. <https://doi.org/10.1109/TLT.2010.33>.

[1]: https://doi.org/10.1109/TLT.2010.33
[boolean-type]: ../types/boolean.md
[boolean-logic]: ../concepts/boolean_logic.md
