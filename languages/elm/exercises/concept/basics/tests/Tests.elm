module Tests exposing (tests)

import Test exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, string)
import Greet exposing (greet)
import String exposing (toUpper)


tests : Test
tests =
    describe "greet"
        [ fuzz (Fuzz.string) "greet should return \"Welcome \" followed by the person to greet, in upper case" <|
            \personToGreet ->
                greet personToGreet
                    |> Expect.equal ("Welcome " ++ toUpper(personToGreet))
        ]
