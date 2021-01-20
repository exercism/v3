In this exercise, you'll be extending the quest logic for the new RPG game a friend is developing.
To make the game more interesting, your friend wants you to add random events that change the situation.

The general setting hasn't changed from the previous exercise.
The new random events are highlighted in the text below.

The game's main character is Annalyn, a brave girl with a fierce and loyal pet dog.
Unfortunately, disaster strikes, as her best friend was kidnapped while searching for berries in the forest.
Annalyn will try to find and free her best friend, optionally taking her dog with her on this quest.

After some time spent following her best friend's trail, she finds the camp in which her best friend is imprisoned. It turns out there are two kidnappers: a mighty knight and a cunning archer.

Having found the kidnappers, Annalyn considers which of the following actions she can engage in:

- _Fast attack_: a fast attack can be made if the knight is sleeping, as it takes time for him to get his armor on, so he will be vulnerable.<!-- **There is a 5% chance that Annalyn stumbles while approaching the knight. This will wake up the knight and buy him enough time to put on his armor so that a fast attack is no longer possible.** -->
- _Spy_: the group can be spied upon if at least one of them is awake. Otherwise, spying is a waste of time. **It is foggy in _half_ the playthroughs, therefore spying is not an option.**
- _Signal prisoner_: the prisoner can be signalled using bird sounds if the prisoner is awake and the archer is sleeping, as archers are trained in bird signaling so they could intercept the message.
- _Free prisoner_: Annalyn can try sneaking into the camp to free the prisoner, but this tactic will only work if the prisoner is awake and the other two characters are sleeping. If the prisoner is sleeping, they'll be startled by Annalyn's sudden appearance and will awaken the other two characters. The prisoner can also be freed if the archer is sleeping and Annalyn has her pet dog with her, as the knight will be scared by the dog and will withdraw, and the archer can't equip his bow fast enough to prevent the prisoner from being freed. **Unfortunately, Annalyn's dog loves chasing after rabbits. There's a 25% chance of the dog encountering a rabbit on the way to the camp. In those situations, the dog is no longer available to scare the knight.**

**After freeing her friend, Annalyn still has some time to loot the camp. She can find anywhere between three to thirteen coins.**

You have five tasks: implement new functions to determine the new random events and adjust existing functions to change their return value depending on the random events.

## Tasks

<!-- TODO: Make sure the name here is consistent with the exercise name -->
!!! note
    You are provided with an example solution to [Annalyn's Analytical Avenge Attempt](https://exercism.io/tracks/julia/exercises/boolean-logic) as a starting point.
    You are free to choose your own solution to that exercise as a starting point instead.

### 1. Add a function that determines if it's foggy

Implement a function named `is_foggy`. This function returns `true` half the time. Otherwise, it returns `false`.

```julia
julia> is_foggy()
true

julia> is_foggy()
false
```

### 2. Check if the 'Spy' action is possible

Adjust the `can_spy` function so that it returns `false` if it `is_foggy()`. Otherwise, the return value follows the existing rules.

```julia
julia> knight_awake = false; archer_awake = true; prisoner_awake = false;

julia> can_spy(knight_awake, archer_awake, prisoner_awake) # it is not foggy
true

julia> can_spy(knight_awake, archer_awake, prisoner_awake) # it is foggy
false
```

### 3. Add a function that determines if the dog has been distracted

Implement a function named `is_dog_distracted`. This function returns `true` 25% of the time. Otherwise, it returns `false`.

```julia
julia> is_dog_distracted()
false

julia> is_dog_distracted()
true

julia> is_dog_distracted()
false
```

### 4. Check if the 'Free Prisoner' action is possible

Adjust the `can_free_prisoner` function so that, depending on the result of `is_foggy()`, the `dog_present` argument is ignored. In those cases, the function should return the same value that it would return if the dog was not present even if it's called with `dog_present = true`. Otherwise, the return value follows the existing rules.

```julia
julia> knight_awake = false; archer_awake = false; prisoner_awake = false; dog_present = true;

julia> can_free_prisoner(knight_awake, archer_awake, prisoner_awake, dog_present) # the dog has not been distracted
true

julia> can_free_prisoner(knight_awake, archer_awake, prisoner_awake, dog_present) # the dog has been distracted
false
```

### 5. Add a looting action

Implement a function named `loot` that returns the number of coins Annalyn finds in the camp. For example:

```julia
julia> loot()
4

julia> loot()
7

julia> loot()
3
```
