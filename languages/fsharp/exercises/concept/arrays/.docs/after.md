An [`array`][arrays] in F# is a mutable collection of zero or more values with a fixed length. This means that once an array has been created, its size cannot change, but its values can. The values in an array must all have the same type. [Arrays can be defined as follows][creating-arrays]:

```fsharp
let empty = [| |]
let emptyAlternative = Array.empty

let singleValue = [| 5 |]
let singleValueAlternative = Array.singleton 5

let threeValues = [| "a"; "b"; "c" |]
```

Elements can be assigned to an array or retrieved from it [using an index][array-indexer]. F# arrays are zero-based, meaning that the first element's index is always zero:

```fsharp
let numbers = [2; 3; 5]

// Update value in array
numbers.[2] <- 9

// Read value from array
numbers.[2]
// => 9
```

Arrays are manipulated by functions and operators defined in the [`Array` module][array-module]. Some of these functions are also available as [properties][array-properties] of an `array` instance:

```fsharp
Array.length [| 7; 8 |] // => 2
[| 7; 8 |].Length       // => 2
```

There is no right or wrong option here. In general, the functions in the `Array` module play better with type inference, whereas the properties allow for more concise code.

The [_array pattern_][array-pattern] allows pattern matching on arrays:

```fsharp
let describe array =
    match array with
    | [| |] -> "Empty"
    | [| 1; 2; three |] -> sprintf "1, 2, %d" three
    | _ -> "Other"

describe [| |]         // => "Empty"
describe [| 1; 2; 4 |] // => "1, 2, 4"
describe [| 5; 7; 9 |] // => "Other"
```

[arrays]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/arrays
[creating-arrays]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/arrays#creating-arrays
[array-indexer]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/arrays#accessing-elements
[array-pattern]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching#array-pattern
[array-module]: https://msdn.microsoft.com/visualfsharpdocs/conceptual/collections.array-module-%5bfsharp%5d
[array-members]: https://docs.microsoft.com/en-us/dotnet/api/system.array?view=netcore-3.1
