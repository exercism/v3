### General

- A [map][maps] is an associative data structure of key-value pairs.
- Elixir offers [many useful Map module functions in the standard library][map-module].

### 1. Define a new high score map

- You need to define a [named function][named-function] with 0 parameters.
- It should return an empty [map][maps].

### 2. Add players to the high score map

- You need to define a [named function][named-function] with 3 parameters.
  - The first argument is a [map][map] of scores.
  - The second argument is a name [string][strings].
  - The third argument is the score [integer value][integers], it is optional, so a [default value][default-arg] should be specified.
- The resulting map should be returned

### 3. Remove players from the score map

- You need to define a [named function][named-function] with 2 parameters.
  - The first argument is a [map][map] of scores.
  - The second argument is a name [string][strings].
- The resulting map should be returned

### 4. Reset a player's score

- You need to define a [named function][named-function] with 2 parameters.
  - The first argument is a [map][map] of scores.
  - The second argument is a name [string][strings].
- The resulting map should be returned with the player's score reset to an initial value.

### 5. Update a player's score

- You need to define a [named function][named-function] with 3 parameters.
  - The first argument is a [map][map] of scores.
  - The second argument is a name [string][strings].
  - The third argument is a score [integer value][integers] to be added to the player's score.
- The resulting map should be returned with the player's updated score.

### 6. Get a list of players with scores ordered by player name

- You need to define a [named function][named-function] with 1 parameters.
  - The first argument is a [map][map] of scores.
- Maps make use of the [Enumerable][enum] protocol.

### 7. Get a list of players ordered by player score in decreasing order

- You need to define a [named function][named-function] with 1 parameters.
  - The first argument is a [map][map] of scores.
- Maps make use of the [Enumerable][enum] protocol.
- Make sure the highest score is listed first in the ordered result.

[maps]: https://elixir-lang.org/getting-started/keywords-and-maps.html#maps
[integers]: https://elixir-lang.org/getting-started/basic-types.html
[strings]: https://elixir-lang.org/getting-started/basic-types.html#strings
[named-function]: https://elixir-lang.org/getting-started/modules-and-functions.html#named-functions
[default-arg]: https://elixir-lang.org/getting-started/modules-and-functions.html#default-arguments
[map-module]: https://hexdocs.pm/elixir/Map.html
[enum]: https://hexdocs.pm/elixir/Enumerable.html#content
