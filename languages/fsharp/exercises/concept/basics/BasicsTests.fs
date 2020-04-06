module BasicsTests

open FsUnit.Xunit
open Xunit

open Basics

[<Fact>]
let ``Expected minutes in oven``() = expectedMinutesInOven |> should equal 40

[<Fact(Skip = "Remove to run test")>]
let ``Remaining minutes in oven``() = remainingMinutesInOven 25 |> should equal 15

[<Fact(Skip = "Remove to run test")>]
let ``Preparation time in minutes for one layer``() = preparationTimeInMinutes 1 |> should equal 2

[<Fact(Skip = "Remove to run test")>]
let ``Preparation time in minutes for multiple layers``() = preparationTimeInMinutes 4 |> should equal 8

[<Fact(Skip = "Remove to run test")>]
let ``Total time in minutes for one layer``() = totalTimeInMinutes 1 30 |> should equal 32

[<Fact(Skip = "Remove to run test")>]
let ``Total time in minutes for multiple layers``() = totalTimeInMinutes 4 8 |> should equal 16
