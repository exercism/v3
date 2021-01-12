The discriminated union type represents a fixed number of named cases. Each value of a discriminated union corresponds to exactly one of the named cases.

A discriminated union is defined using the `type` keyword, with cases separated by pipe (`|`) characters:

```fsharp
type Season =
    | Spring
    | Summer
    | Autumn
    | Winter
```

Each case of a discriminated union can optionally have data associated with it, and different cases can have different types of data. If none of the cases have data associated with them, the discriminated union is similar to what other languages usually refer to as an _enumeration_ (or _enum_).

```fsharp
type Number =
    | Integer of int
    | Float of float
    | Invalid
```

Creating a value for a specific case can be done by referring to its name (e.g, `Success`). As case names are just constructor functions, associated data can be passed as a regular function argument. If another discriminated union has defined a case with the same name, you'll need to use its full name (e.g. `Result.Succes`).

```fsharp
let byName = Integer 2
let byFullName = Number.Invalid
```

Discriminated unions have _structural equality_, which means that two values for the same case and with the same (optional) data are equivalent.

While one can use `if/elif/else` expressions to work with discriminated unions, the recommended way to work with them is through pattern matching using the _identifier pattern_:

```fsharp
let describe number =
    match number with
    | Integer i -> sprintf "Integer: %d" i
    | Float d  -> sprintf "Float: %d" i
    | Invalid   -> "Invalid"
```
