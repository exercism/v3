module ValuesTests

open FsUnit.Xunit
open Xunit

open Values

[<Fact>]
let ``Expected minutes in oven``() = expectedMinutesInOven |> should equal 40

[<Fact>]
let ``Remaining minutes in oven``() = remainingMinutesInOven 25 |> should equal 15

[<Fact>]
let ``Preparation time in minutes for one layer``() = preparationTimeInMinutes 1 |> should equal 2

[<Fact>]
let ``Preparation time in minutes for multiple layers``() = preparationTimeInMinutes 4 |> should equal 8

[<Fact>]
let ``Total time in minutes for one layer``() = totalTimeInMinutes 1 30 |> should equal 32

[<Fact>]
let ``Total time in minutes for multiple layers``() = totalTimeInMinutes 4 8 |> should equal 16