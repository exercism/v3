As a child, you loved playing Monopoly. While as an adult you might prefer board games with more strategy and less luck involved, you decided to dust off the rulebook and have fun implementing the game in Elixir. You start with the dice rolls.

In Monopoly, players roll two 6-sided dice to determine their fate. If they roll the same value on both dice, they can roll again. However, if they get unlucky and roll doubles 3 times in a row, they go directly to jail!

## 1. Do one 6-sided die roll

Implement the `d6/0` function that will randomly choose one integer between 1 and 6 on each call.

```elixir
Monopoly.d6()
# => 1
Monopoly.d6()
# => 5
```

## 2. Do one roll of two 6-sided dice

Implement the `roll/0` function. It should return a stream. When converted to a list, the stream should return a 1-element list with a 2-tuple, each value in the tuple being a randomly chosen integer between 1 and 6. Use the `d6/0` function defined in the previous step.

```elixir
Monopoly.roll() |> Enum.to_list()
# => [{1, 5}]
Monopoly.roll() |> Enum.to_list()
# => [{5, 2}]
```

## 3. Roll again after a double.

Extend the `roll/0` function. If the previous roll of two 6-sided dice resulted in both dice rolling the same number, do another two 6-sided dice roll. Stop rolling after 3 rolls, regardless of the result.

```elixir
Monopoly.roll() |> Enum.to_list()
# => [{4, 4}, {2, 4}]
Monopoly.roll() |> Enum.to_list()
# => [{6, 6}, {1, 1}, {3, 3}]
```
