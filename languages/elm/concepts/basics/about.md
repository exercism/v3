## Minimal compilable file

### Modules

Each file in Elm is a module, and must contain a `module` statement before all other code.

Everything is scoped to the module, unless listed in `exposing`

Module names must match their file name, so module `Greet` must be in file Greet.elm.

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
import List                            -- List.map, List.foldl
import List as L                       -- L.map, L.foldl

-- open imports
import List exposing (..)              -- map, foldl, concat, ...
import List exposing ( map, foldl )    -- map, foldl
```

https://elm-lang.org/docs/syntax#modules

### Functions

Invoking a function is done by specifying its name and passing arguments for each of the function's parameters.

```elm
let five = add 2 3
```

To pass expressions as parameters, surround them in brackets

```elm
let six = add 2 (add 3 1)
```

Functions are defined with Name, Parameters, "=", Expressions. Functions automatically return their last expression. 

```elm
add number1 number2 = 
	number1 + number2
```

The Elm compiler uses Type Inference to work out the Type of the function, but the [style guide](https://elm-lang.org/docs/style-guide) states that all top level functions should have Type Annotations.

Type Annotations are defined with Function Name, ":", Parameter Types separated by "->", Return Type.

```elm
add : Int -> Int -> Int
add number1 number2 = 
	number1 + number2
```

https://guide.elm-lang.org/types/reading_types.html

### Indentation / significant whitespace

Elm doesn’t use syntactic markers such as curly brackets, parentheses, or semicolons to specify code boundaries. It uses whitespaces and indentations instead.

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

