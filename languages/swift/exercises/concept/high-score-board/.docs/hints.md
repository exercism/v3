## 1. Define a new high score dictionary

- Empty dictionaries can be initialized by following the type name with a pair of empty parentheses or, if the type can be determined from context, by using the empty dictionary literal.

## 2. Add players to the high score dictionary

- A dictionary must be declared as a variable in order to add elements to it.
- Elements can be added to a dictionary using the [dictionary subscript syntax][dictionary-subscripts].

## 3. Remove players from the score dictionary

- A dictionary must be declared as a variable in order to remove elements from it.
- Elements can be removed from a dictionary using the [appropriate method][dictionary-docs].

## 4. Reset a player's score

- A dictionary must be declared as a variable in order to change values in it.
- Values can be updated in a dictionary using the [dictionary subscript syntax][dictionary-subscripts].

## 5. Update a player's score

- A dictionary must be declared as a variable in order to change values in it.
- Values can be updated in a dictionary using the [dictionary subscript syntax][dictionary-subscripts].

## 6. Get a list of players with scores ordered by player name

- Dictionary elements can be sorted using the [appropriate method][dictionary-docs].
- You will need to define a sorting function of type `((String, Int), (String, Int)) -> Bool` to pass in to the `sorted(by:)` method.
- The sorting function should return `true` if the left-hand key/value pair should appear _before_ the right-hand key/value pair in the sorted output.

## 7. Get a list of players ordered by player score in decreasing order

- Dictionary elements can be sorted using the [appropriate method][dictionary-docs].
- You will need to define a sorting function of type `((String, Int), (String, Int)) -> Bool` to pass in to the `sorted(by:)` method.
- The sorting function should return `true` if the left-hand key/value pair should appear _before_ the right-hand key/value pair in the sorted output.

[dictionaries]: https://docs.swift.org/swift-book/LanguageGuide/CollectionTypes.html#ID113
[dictionary-docs]: https://developer.apple.com/documentation/swift/Dictionary
[dictionary-subscripts]: https://docs.swift.org/swift-book/LanguageGuide/CollectionTypes.html#ID116
