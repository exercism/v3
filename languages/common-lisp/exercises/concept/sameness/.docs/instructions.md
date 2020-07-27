Leslie the Lisp Alien is programming their maze-robot (as young Lisp Aliens are wont to do) and needs some help. The maze consists of a series of rooms. Each room as a choices of doors. Each door needs a key. If you use the wrong key at a door it is likely to explode (do not worry for the robot, there will be no damage but buffing out the scorch marks is quite annoying for Leslie). If the robot doesn't have a key that opens any doors then the whole room will explode! (Again, do not worry it will not damage but will be annoying to clean.)

What you need to help Leslie with is to give the robot the correct key for each room. The robot will use the key on each door in the room in order and go through the first door that opens.

Each test represents a maze and each assertion is a room. You will need to write predicate functions that evaluate to `T` when the arguments are equal and `NIL` when they are not.

## 1. The maze of object equality

This maze has only a single room. You need to provide a function that can tell if two objects are the same object.

For example:

```lisp
a ; => "pizza"
b ; => "pizza"

(key-object-equality a a) ; => T
(key-object-equality a b) ; => NIL
```

because while `a` and `b` appear to be the same, they are not the same object.

## 2. The maze of numbers

This maze has two rooms. The first has a door that can be opened if your key can tell if two numbers are strictly equal to each other. The second room needs a key that allows some flexibility on checking for equal numbers.

For example:

```lisp
a ; => 13
b ; => 13.0
c ; => 13

(key-numbers a b) ; => NIL
(key-numbers a c) ; => T
(key-numbers-of-different-types a b) ; => T
```

because while `a` and `b` are numerically the same they are of different types.

## 3. The maze of characters

This maze has two rooms. The first has a door that can be opened if your key can tell if two characters are strictly equal to each other. The second room needs a key that allows for case-insensitive comparison of the characters.

For example:

```lisp
a ; => #\X
b ; => #\x
c ; => #\X

(key-characters a b) ; => NIL
(key-characters a c) ; => T
(key-characters-case-insensitively a b) ; => T
```

because only very permissive equality predicates ignore case when comparing two characters.

## 4. The maze of strings

Like the maze of characters the first room needs a key that checks if two strings are equal; the second room needs a key that allows for case-insensitive comparison.

For example:

```lisp
a ; => "pizza"
b ; => "PIZZA"
c ; => "pizza"

(key-strings a b) ; => NIL
(key-strings a c) ; => T
(key-strings-case-insensitively a b) ; => T
```

because only very permissive equality predicates ignore case when comparing two strings.

## 5. The maze of conses

This is a big maze with many rooms. Each room needs a key that will check if the conses _contain_ the things which are equal. Each room will check for equality of conses with different contents: symbols, numbers, characters, and some will need more flexible equality definitions for those contents.

For example:

```lisp
syms-a ; => (left . right)
syms-b ; => (up . down)
syms-c ; => (left . right)
chars-a ; => (#\x . #\y)
chars-b ; => (#\x . #\Y)
chars-c ; => (#\x . #\y)
nums-a ; => (13 . 23)
nums-b ; => (13 . 23.0)
nums-c ; => (13 . 23)


(key-conses-of-symbols syms-a syms-b) ; => NIL
(key-conses-of-symbols syms-a syms-c) ; => T

(key-conses-of-characters chars-a chars-b) ; => NIL
(key-conses-of-characters chars-a chars-c) ; => T
(key-conses-of-characters-case-insensitively chars-a chars-b) ; => T

(key-conses-of-numbers nums-a nums-b) ; => NIL
(key-conses-of-numbers nums-a nums-c) ; => T
(key-conses-of-numbers-of-different-types nums-a nums-b) ; => T
```

because even with an equality predicate that will look inside of a cons, it may still not be permissive enough to ignore case or numeric type when comparing.

## 6. The maze of arrays

This maze is simpler with only two rooms. The first needs a key that checks if the arrays contain the equal contents. The second needs a key that is more flexible about checking equality of numbers.

For example:

```lisp
a ; => #[13 23]
b ; => #[13 23.0]

(key-arrays a b) ; => NIL
(key-arrays-loosely a b) ; => T
```

because a permissive equality predicate will not consider numeric type when comparing the contents of an array.
