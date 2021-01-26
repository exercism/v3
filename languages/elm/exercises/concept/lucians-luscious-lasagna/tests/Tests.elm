module Tests exposing (tests)

import Cook exposing (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes)
import Expect
import Fuzz
import Test exposing (..)


tests : Test
tests =
    describe "Cook"
        [ test "expectedMinutesInOven" <|
            \_ ->
                expectedMinutesInOven
                    |> Expect.equal 40
        , skip <|
            fuzz positiveInt "preparationTimeInMinutes" <|
                \layers ->
                    preparationTimeInMinutes layers
                        |> Expect.equal (2 * layers)
        , skip <|
            fuzz (Fuzz.tuple ( positiveInt, positiveInt )) "elapsedTimeInMinutes" <|
                \( layers, passedAlready ) ->
                    elapsedTimeInMinutes layers passedAlready
                        |> Expect.equal (2 * layers + passedAlready)
        ]


positiveInt =
    Fuzz.intRange 0 1000000
