A `list` in F# is an immutable collection of zero or more values. The values in a list must all have the same type.

The first item in a list is referred to as the _head_, while everything after the first item is referred to as the _tail_.

Lists are manipulated by functions and operators defined in the `List` module. Once a list has been constructed, its value can never change. Any functions that appear to modify a list (such as adding an element), will actually return a new list.

As lists are implemented as singly linked lists, adding a new item to a list means adding it to the beginning of the list (before the existing elements in the list). Consequently, accessing the first item of the list is very fast, but accessing an element by index is relatively slow.

Here are some examples on how to define a list:

```fsharp
let empty = []
// => []

let singleElement = [1]
// => [1]

let multipleElements = [2; 3; 4]
// => [2; 3; 4]

let prependElement = 5 :: [6; 7; 8]
// => [5; 6; 7; 8]
```
