-- This line 'exports' the `greet` function, so that 
-- other modules (such as the tests) can use it
module Greet exposing (greet)
-- or
-- module Basics exposing (..)

-- you will need an `import` statement here to import the `toUpper` from the `String` module
-- import String exposing (toUpper)
-- or
-- import String exposing (..)

greet : String -> String
greet personToGreet =
    Debug.todo "Implement greet"
