In this exercise, you'll be extending the quest logic for the new role-playing game a friend is developing.
To make the game more interesting, your friend wants you to add random events that change the situation.

The general setting hasn't changed from the previous exercise.
The new random events are highlighted in the text below.

The game's main character is Annalyn, a brave girl with a fierce and loyal pet dog.
Unfortunately, disaster strikes, as her best human friend was kidnapped while searching for berries in the forest.
Annalyn will try to find and free her best friend, optionally taking her dog with her on this quest.

After some time spent following her best friend's trail, she finds the camp in which her best friend is imprisoned. It turns out there are two kidnappers: a mighty knight and a cunning archer.

Having found the kidnappers, Annalyn considers which of the following actions she can engage in:

- _Fast attack_: a fast attack can be made if the knight is sleeping, as it takes time for him to get his armor on, so he will be vulnerable.<!-- **There is a 5% chance that Annalyn stumbles while approaching the knight. This will wake up the knight and buy him enough time to put on his armor so that a fast attack is no longer possible.** [potential future test case] -->
- _Spy_: the group can be spied upon if at least one of them is awake. Otherwise, spying is a waste of time. **It is foggy in _half_ the playthroughs, therefore spying is not an option.**
- _Signal prisoner_: the prisoner can be signalled using bird sounds if the prisoner is awake and the archer is sleeping, as archers are trained in bird signaling so they could intercept the message.
- _Free prisoner_: Annalyn can try sneaking into the camp to free the prisoner, but this tactic will only work if the prisoner is awake and the other two characters are sleeping. If the prisoner is sleeping, they'll be startled by Annalyn's sudden appearance and will awaken the other two characters. The prisoner can also be freed if the archer is sleeping and Annalyn has her pet dog with her, as the knight will be scared by the dog and will withdraw, and the archer can't equip his bow fast enough to prevent the prisoner from being freed. **Unfortunately, Annalyn's dog loves chasing after rabbits. There's a 25% chance of the dog encountering a rabbit on the way to the camp. In those situations, the dog is no longer available to scare the knight.**

**After freeing her friend, Annalyn still has some time to quickly loot the camp. She can find anywhere between three and thirteen coins. She also comes across a treasure chest with a random item inside.**

Your tasks are to implement the functions that determine if these random events occur, and to determine Annalyn's loot.

## Tasks

### 1. Add a function that determines if it's foggy

Implement a function named `is_foggy`. This function returns `true` half the time. Otherwise, it returns `false`.

```julia
julia> is_foggy()
true

julia> is_foggy()
false
```

### 2. Add a function that determines if the dog has been distracted

Implement a function named `is_dog_distracted`. This function returns `true` 25% of the time. Otherwise, it returns `false`.

```julia
julia> is_dog_distracted()
false

julia> is_dog_distracted()
true

julia> is_dog_distracted()
false
```

### 3. Add a looting action for coins

Implement a function named `loot` that returns the number of coins Annalyn finds in the camp. For example:

```julia
julia> loot()
4

julia> loot()
7

julia> loot()
3
```

### 4. Add a looting action for the treasure chest

Implement a function named `loot` that takes a collection of items as an argument and returns the item Annalyn finds in the chest. For example:

```julia
julia> loot(["Cabbage", "Daring Dagger", "Sneaky Shoes"])
"Cabbage"

julia> loot(["Cabbage", "Daring Dagger", "Sneaky Shoes"])
"Sneaky Shoes"
```
