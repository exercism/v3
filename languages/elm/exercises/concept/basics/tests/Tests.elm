module Tests exposing (tests)

import Basics exposing (..)
import Expect
import Test exposing (..)


tests : Test
tests =
    describe "TODO"
        [ test "test 1" <|
            \() ->
                Basics.todo "todo"
                    |> Expect.equal (Debug.todo "todo")
        ]
