The discriminated union type represents a fixed number of named cases. Each value of a discriminated union corresponds to exactly one of the named cases.

Each case of a discriminated union can optionally have data associated with it, and different cases can have different types of data. If none of the cases have data associated with them, the discriminated union is similar to what other languages usually refer to as an _enumeration_ (or _enum_).

A discriminated union is defined using the `type` keyword, with cases separated by pipe (`|`) characters:

```fsharp
type Result =
    | Success
    | Error of string
```

Creating a value for a specific case can be done by referring to its name (e.g, `Success`). As case names are just constructor functions, associated data can be passed as a regular function argument.

If another discriminated union has defined a case with the same name, you'll need to use its full name (e.g. `Result.Succes`).

Discriminated unions have _structural equality_, which means that two values for the same case and with the same (optional) data are equivalent.

Discriminated unions are a key data type in F# and you'll see them being used in many places (including the base library).

While one can use `if/elif/else` expressions to work with discriminated unions, the recommended way to work with them is through pattern matching.

Pattern matching is a way in which a value can be tested against one or more _patterns_. These patterns can take different forms, depending on the type of value that is passed. Examples of patterns are constant patterns, which match a constant value (e.g. `1` or `"hello"`), and identifier patterns, which match on an identifier (like a discriminated union case).

Pattern matching is similar to switch statements in other languages, although most switch statements only support constant patterns.

In F#, pattern matching is done through the `match` keyword. which defines a `match` expression. The `match` expression takes the value to pattern match on as its argument and defines one or more _patterns_ to test the value against. The value is tested against each pattern from top to bottom, until it finds one that matches and then execute the logic associated with the pattern (the order of patterns thus matters!). In some cases, you may want to add an additional condition to a pattern. This is knows as a _guard_ (clause), which can be added using the `when` keyword.

In many cases, the F# compiler can detect if a `match` expression handles _all_ possible cases. For example, if one forgets to match one case of a discriminated union, the compiler will output a warning. One can then either explicitly match on the missing case, or use a wildcard pattern (using the `_` character), which is like a _catch-all_ pattern and is normally the last pattern in a `match` expression.
