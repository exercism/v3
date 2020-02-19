# Instructions

In this exercise we will simulate the first turn of a Blackjack game.

You will receive two cards and will be able to see the face up card of the dealer. All cards are represented using a string such as "ace", "king", "three", "two", etc.

Depending on your two cards and the card of the dealer, there is an optimal strategy for the first turn of the game, in which you have the following options:

    - Stand (S)
    - Hit (H)
    - Double (D)
    - Split (P)
    - Automatically win (W)

This strategy is as follows:

- If you have a Blackjack and the dealer does not have an ace, a figure or a ten then you automatically win. If the dealer does have any of those cards then you'll have to stand and wait for the reveal of the other card.
- If you have a pair of cards you must always split them, except if the pair is made up of any figures or tens, in which case you should stand.
- If your cards sum up to 17 or higher you should always stand.
- If your cards sum up to 8 or lower you should always hit.
- For every other case the following chart (your hand total shown in the rows and the dealer's card in the columns) shows the best strategy:

|    | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | A |
|----|---|---|---|---|---|---|---|---|----|---|
| 16 | S | S | S | S | S | H | H | H | H  | H |
| 15 | S | S | S | S | S | H | H | H | H  | H |
| 14 | S | S | S | S | S | H | H | H | H  | H |
| 13 | S | S | S | S | S | H | H | H | H  | H |
| 12 | H | H | S | S | S | H | H | H | H  | H |
| 11 | D | D | D | D | D | D | D | D | D  | D |
| 10 | D | D | D | D | D | D | D | D | D  | D |
| 9  | H | D | D | D | D | H | H | H | H  | H |