## What is one supposed to do again?

The robot is asked to provide a key for a room. A key should be one of the equality generic equality predicates (`eq`, `eql`, `equal`, `equalp`). Each room is a list of pairs of doors and what's being them (`'explosion' or`'victory`). A door is pair of items that the equality predicate will be called with.

So for a room defined as `((("foo" "FOO") . 'explosion) ((1 1) . 'victory))` you might want to provide `eql` as the key.

## Why does the room keep exploding!?

If you keep getting `'room-explodes` instead of `'victory` (or even `'explosion`) that means that your robot is not returning an equality predicate that works for any of the doors of the room.

First thing to check is that your specified the room id correctly in the `case` expression in your robot. The robot has a default case of returning a predicate that will not open any door.

## Doors keep exploding!

If you give the wrong equality predicate to a room it might open the wrong door. This is usually will happen if, for example you specify `equalp` when `equal` is correct. `equalp` is more permissive and thus will open more doors.

Eli Bendersky has [an informative page][lisp-equality] about the generic equality predicates which might be useful to refer to.

[lisp-equality]: https://eli.thegreenplace.net/2004/08/08/equality-in-lisp
