## Minimal compilable file

### Modules

Each file in Elm is a module, and must contain a `module` statement before all other code.

Module names must match their file name, so module `Greet` must be in file Greet.elm.

Anything defined within a module is privatly scoped to it
and cannot be accessed from outside this module, unless listed in `exposing`.

```elm
-- Define the Greet module, and expose the `greet` function
module Greet exposing (greet)
```

```elm
-- Define the Greet module, and expose everything
module Greet exposing (..)
```

https://elm-lang.org/docs/syntax#modules

### Imports

You must `import` functions from other modules in order to use them. Qualified imports are preferred.

```elm
-- qualified imports
import List                             -- List.repeat, List.length
import List as L                        -- L.repeat, L.length

-- open imports
import List exposing (..)               -- repeat, length, concat, ...
import List exposing ( repeat, length ) -- repeat, length
```

https://elm-lang.org/docs/syntax#modules

### Constants and functions

A constant value is defined with `name = expression`,
where in elm, everything except definitions are expressions.

```elm
five = 5
six = 3 + 3
```

Functions are defined with `name parameters... = expression`,
parameters being only separated by space.

```elm
add number1 number2 = number1 + number2
```

Invoking a function also is an expression and is done by
specifying its name and passing arguments for each of the function's parameters,
separated by space, just like for function definition.

```elm
five = add 2 3
```

If evaluation of an expression is ambiguous,
parentheses can be used to specify the order of evaluation.

```elm
six = add 2 (add 3 1)
```

### Indentation / significant whitespace

Elm doesn't use syntactic markers such as curly brackets, parentheses, or semicolons to specify code boundaries. It uses whitespaces and indentations instead.

- Module, import, and top-level function definitions must start at the left most column.
- If an expression is split into multiple lines, the code that is part of that expression must be indented under that expression with at least one space.
- Parts of the expression that are grouped together should be indented with equal number of spaces.

```elm
-- A function split over multiple lines, so subsequent lines must be indented
greet personToGreet =
    Debug.todo "Implement greet"
```

https://elmprogramming.com/indentation.html

### Comments

```elm
-- a single line comment

{- a multiline comment
   {- can be nested -}
-}
```

### Formatting

There is a [style guide](https://elm-lang.org/docs/style-guide), and [elm-format](https://github.com/avh4/elm-format) can be used to automatically format code.
