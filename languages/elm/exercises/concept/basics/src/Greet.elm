-- This line 'exports' the `greet` function, so that 
-- other modules (such as the tests) can use it
module Greet exposing (greet)
-- It is possible to export everything with `module Greet exposing (..)`

-- you will need an `import` statement here to import the `toUpper` function from the `String` module
-- import String exposing (toUpper)
-- It is possible to import everything from the String module with `import String exposing (..)`

greet : String -> String
greet personToGreet =
    Debug.todo "Implement greet"
