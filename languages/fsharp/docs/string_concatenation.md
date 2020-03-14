# String concatenation

The simplest way to concatenate two strings is to use the `+` operator:

```fsharp
"abso" + "lutely"
// => "absolutely"
```

This works just as well with string values assigned to identifiers:

```fsharp
let vehicle = "bike"
let adjective = "shiny"
adjective + " " + vehicle
// => "shiny bike"
```

While the `+` operator is easy to use, for anything but trivial string concatenation, the `sprintf` function is preferred.

The `sprintf` function takes a _format string_, which is a template for the string to return. All variable parts in the template are defined using placeholders. The values replacing the placeholders are passed as arguments.

The placeholder to use for strings is `%s`:

```fsharp
let book = "Dune"
let author = "Frank Herbert"
sprintf "My favorite book is %s by %s." book author
// => "My favorite book is Dune by Frank Herbert."
```
