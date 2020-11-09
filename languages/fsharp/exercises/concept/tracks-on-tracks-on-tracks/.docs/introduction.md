## lists

A `list` in F# is an immutable collection of zero or more values. The values in a list must all have the same type. As lists are immutable, once a list has been constructed, its value can never change. Any functions/operators that appear to modify a list (such as adding an element), will actually return a new list.

Lists can be defined as follows:

```fsharp
let empty = []
let singleValue = [5]
let threeValues = ["a"; "b"; "c"]
```

The most common way to add an element to a list is through the `::` (cons) operator:

```fsharp
let twoToFour = [2; 3; 4]
let oneToFour = 1 :: twoToFour
// => [1; 2; 3; 4]
```

F# list is have a _head_ (the first element) and a _tail_ (everything after the first element). The tail of a list is itself a list.

Lists are either manipulated by functions and operators defined in the `List` module, or manually using pattern matching using the _list_ and _cons_ patterns:

```fsharp
let rec describe list =
    match list with
    | [] -> "Empty list"
    | head::tail -> sprintf "Non-empty list with head: %d" head

describe []        // => "Empty list"
describe [1]       // => "Non-empty with head: 1"
describe [5; 7; 9] // => "Non-empty with head: 5"
```
