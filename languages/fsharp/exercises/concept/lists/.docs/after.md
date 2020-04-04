A [`list`][lists] in F# is an immutable collection of zero or more values. The values in a list must all have the same type. As lists are immutable, once a list has been constructed, its value can never change. F# list have a _head_ (the first element) and a _tail_ (everything after the first element). The tail of a list is itself a list.

Lists are manipulated by functions and operators defined in the [`List` module][list-module]. Some of these functions are also available as [list-properties][list-properties] of a `list` instance.

Any functions/operators that appear to modify a list (such as adding an element), will actually return a new list. Performance is usually not an issue though, as the implementation of lists prevents unnecessary allocations/copies.

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

List can be appended using the `@` operator:

```fsharp
[6; 7] @ [8; 9]
// => [6; 7; 8; 9]
```

As lists are implemented as singly linked lists, prepending an element (using the `::` operator) is very fast, while accessing an element by index is potentially relatively slow.

List can also be processed using pattern matching through the [_list_][list-pattern] and [_cons_][cons-pattern] patterns:

```fsharp
let rec describe list =
    match list with
    | [] -> "Empty list"
    | head::tail -> sprintf "Non-empty list with head: %d" head

describe []        // => "Empty list"
describe [1]       // => "Non-empty with head: 1"
describe [5; 7; 9] // => "Non-empty with head: 5"
```

[lists]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/lists
[list-module]: https://msdn.microsoft.com/visualfsharpdocs/conceptual/collections.list-module-%5Bfsharp%5D?f=255&MSPPError=-2147217396
[list-properties]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/lists#properties
[cons-pattern]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching#cons-pattern
[list-pattern]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching#list-pattern
