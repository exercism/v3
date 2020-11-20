## arrays

An `array` in F# is a mutable collection of zero or more values with a fixed length. This means that once an array has been created, its size cannot change, but its values can. The values in an array must all have the same type. Arrays can be defined as follows:

```fsharp
let empty = [| |]
let emptyAlternative = Array.empty

let singleValue = [| 5 |]
let singleValueAlternative = Array.singleton 5

let threeValues = [| "a"; "b"; "c" |]
```

Elements can be assigned to an array or retrieved from it using an index. F# arrays are zero-based, meaning that the first element's index is always zero:

```fsharp
let numbers = [| 2; 3; 5 |]

// Update value in array
numbers.[2] <- 9

// Read value from array
numbers.[2]
// => 9
```

Arrays are either manipulated by functions and operators defined in the `Array` module, or manually using pattern matching using the _array_ pattern:

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
