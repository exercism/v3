Leslie the Lisp Alien is programming their maze-robot (as young Lisp Aliens are wont to do) and needs some help. The maze consists of a series of rooms. Each room as a choices of doors. Each door needs a key. If you use the wrong key at a door it is likely to explode (do not worry for the robot, there will be no damage but buffing out the scorch marks is quite annoying for Leslie). If the robot doesn't have a key that opens any doors then the whole room will explode! (Again, do not worry it will not damage but will be annoying to clean.)

What you need to help Leslie with is to give the robot the correct key for each room. The robot will use the key on each door in the room in order and go through the first door that opens.

Each test represents a maze and each assertion is a room. You will need to write predicate functions that evaluate to `T` when the arguments are equal and `NIL` when they are not.

## 1. The maze of object equality

This maze has only a single room. You need to provide a function that can tell if two objects are the same object.

For example if `a` is `"pizza"` and `b` is `"pizza"` then `(eq a b)` is `NIL` because while they look the same they are not the same object. But `(eq a a)` is `T` because it is the same object.

## 2. The maze of numbers

This maze has two rooms. The first has a door that can be opened if your key can tell if two numbers are strictly equal to each other. The second room needs a key that allows some flexibility on checking for equal numbers.

For example if `a` is `13` and `b` is `13.0` then `(eql a b)` will be `NIL` while `(equalp a b)` will be `T` because while they have the same numeric value they are of different numeric _types_.

## 3. The maze of characters

This maze has two rooms. The first has a door that can be opened if your key can tell if two characters are strictly equal to each other. The second room needs a key that allows for case-insensitive comparison of the characters.

For example if `a` is `#\X` and `b` is `#x` and c is `#\X` then `(eql a b)` will be `NIL` while `(eql a c)` and `(equalp a b)` will be `T` because only `equalp` does case-insensitive equality checking.

## 4. The maze of strings

Like the maze of characters the first room needs a key that checks if two strings are equal; the second room needs a key that allows for case-insensitive comparison.

For example if `a` is `"pizza"` and `b` is `"PIZZA"` and `c` is `"pizza"` then `(equal a b)` will be `NIL` while `(equal a c)` and `(equalp a b)` will be `T` because only `equalp` does case-insensitive equality checking.

## 5. The maze of conses

This is a big maze with many rooms. Each room needs a key that will check if the conses _contain_ the things which are equal. Each room will check for equality of conses with different contents: symbols, numbers, characters, and some will need more flexible equality definitions for those contents.

For example if `a` is `(13 . 23)` and `b` is `(13 . 23.0)` then `(equal a b)` will be `NIL` while `(equalp a b)` will be `T` because while the contents are numerically similar the elements have different types.

## 6. The maze of arrays

This maze is simpler with only two rooms. The first needs a key that checks if the arrays contain the equal contents. The second needs a key that is more flexible about checking equality of numbers.

For example if `a` is `#[13 . 23]` and `b` is `#[13 . 23]` then `(equal a b)` will be `NIL` while `(equalp a b)` will be `T` because `equal` only compares (non-string) arrays with `eq`. Only `equalp` will look at the contents of an array.
