module BirdWatcherTests

open FsUnit.Xunit
open Xunit

open BirdWatcher

[<Fact>]
let ``Last week`` () =
    lastWeek |> should equal [| 0; 2; 5; 3; 7; 8; 4 |]

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Yesterday's bird count of disappointing week`` () =
    yesterday [| 0; 0; 2; 0; 0; 1; 0 |]
    |> should equal 1

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Yesterday's bird count of busy week`` () =
    yesterday [| 8; 8; 9; 5; 4; 7; 10 |]
    |> should equal 7

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Total number of birds of disappointing week`` () =
    total [| 0; 0; 2; 0; 0; 1; 0 |] |> should equal 3

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Total number of birds of busy week`` () =
    total [| 5; 9; 12; 6; 8; 8; 17 |]
    |> should equal 65

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Day without birds for week that had day without birds`` () =
    dayWithoutBirds [| 5; 5; 4; 0; 7; 6; 7 |]
    |> should equal true

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Day without birds for week that did not have day without birds`` () =
    dayWithoutBirds [| 4; 5; 9; 10; 9; 4; 3 |]
    |> should equal false

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Increment today's count with no previous visits`` () =
    let birdCounts = [| 6; 5; 5; 11; 2; 5; 0 |]
    incrementTodaysCount birdCounts
    |> should equal [| 6; 5; 5; 11; 2; 5; 1 |]

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Increment today's count with multiple previous visits`` () =
    let birdCounts = [| 5; 2; 4; 2; 4; 5; 7 |]
    incrementTodaysCount birdCounts
    |> should equal [| 5; 2; 4; 2; 4; 5; 8 |]

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Odd week for week matching odd pattern`` () =
    oddWeek [| 1; 0; 1; 0; 1; 0; 1 |]
    |> should equal true

[<Fact(Skip = "Remove this Skip property to run this test")>]
let `` Odd week for week that does not match odd pattern`` () =
    oddWeek [| 2; 2; 1; 0; 1; 1; 1 |]
    |> should equal false
