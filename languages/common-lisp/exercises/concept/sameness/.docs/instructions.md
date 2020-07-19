Leslie the Lisp Alien is programming their maze-robot (as young Lisp Aliens are wont to do) and needs some help. The maze consists of a series of rooms. Each room as a choices of doors. Each door needs a key. If you use the wrong key at a door it is likely to explode (do not worry for the robot, there will be no damage but buffing out the scorch marks is quite annoying for Leslie). If the robot doesn't have a key that opens any doors then the whole room will explode! (Again, do not worry it will not damage but will be annoying to clean.)

What you need to help Leslie with is to give the robot the correct key for each room. The robot will use the key on each door in the room in order and go through the first door that opens.

Each test represents a maze and each assertion is a room. You will need to write predicate functions that evaluate to `T` when the arguments are equal and `NIL` when they are not.

## The Maze of Object Equality

This maze has only a single room. You need to provide a function that can tell if two objects are the same object.

## The Maze of Numbers

This maze has two rooms. The first has a door that can be opened if your key can tell if two numbers are strictly equal to each other. The second room needs a key that allows some flexibility on checking for equal numbers.

## The Maze of Characters

This maze has two rooms. The first has a door that can be opened if your key can tell if two characters are strictly equal to each other. The second room needs a key that allows for case-insensitive comparison of the characters.

## The Maze of Strings

Like the maze of characters the first room needs a key that checks if two strings are equal; the second room needs a key that allows for case-insensitive comparison.

## The Maze of Conses

This is a big maze with many rooms. Each room needs a key that will check if the conses _contain_ the things which are equal. Each room will check for equality of conses with different contents: symbols, numbers, characters, and some will need more flexible equality definitions for those contents.

## The Maze of Arrays

This maze is simpler with only two rooms. The first needs a key that checks if the arrays contain the equal contents. The second needs a key that is more flexible about checking equality of numbers.
