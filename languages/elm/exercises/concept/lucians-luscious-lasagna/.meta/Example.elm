module Greet exposing (greet)

import String exposing (toUpper)


greet : String -> String
greet personToGreet =
    "Welcome " ++ toUpper(personToGreet)
