module Tests exposing (tests)

import Cook exposing (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes)
import Expect
import Fuzz
import Random
import Test exposing (..)


tests : Test
tests =
    describe "Cook"
        [ test "expectedMinutesInOven" <|
            \_ ->
                expectedMinutesInOven
                    |> Expect.equal 40
        , skip <|
            fuzz (Fuzz.intRange 0 Random.maxInt) "preparationTimeInMinutes" <|
                \layers ->
                    preparationTimeInMinutes layers
                        |> Expect.equal (2 * layers)
        , skip <|
            fuzz (Fuzz.intRange 0 Random.maxInt) "preparationTimeInMinutes" <|
                \layers ->
                    elapsedTimeInMinutes layers passedAlready
                        |> Expect.equal (2 * layers + passedAlready)
        ]
