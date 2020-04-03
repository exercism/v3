A `list` in F# is an immutable collection of zero or more values. The values in a list must all have the same type. Lists are implemented as singly linked lists.

To define a list, list the elements separated by semicolons (`;`) and enclose it in square brackets (`[]`). The most common way to add an item to a list it through the cons operator (`::`), which adds an element to the beginning of the list and is very fast.

These are some examples on how to define a list:

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

The first item of a list is referred to as the _head_, with the items after the first item being referred to as the _tail_.

Lists are manipulated by functions and operators defined in the [`List` module][module]. Some of these functions are also available as [properties][properties] of a `list` instance.

As lists are immutable, any functions that appear to modify a list, will actually return a new list. Performance is usually not an issue though, as the implementation of lists prevents unnecessary allocations/copies.

```fsharp
let list1 = [1; 2; 3]
let list2 = 4 :: list1

list1 = list2
// => false
```

[module]: https://msdn.microsoft.com/visualfsharpdocs/conceptual/collections.list-module-%5Bfsharp%5D?f=255&MSPPError=-2147217396
[properties]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/lists#properties
