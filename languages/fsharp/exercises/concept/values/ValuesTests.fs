module ValuesTests

open FsUnit.Xunit
open Xunit

open Values

[<Fact>]
let ``Expected minutes in oven``() = expectedMinutesInOven |> should equal 40

[<Fact>]
let ``Actual minutes in oven``() =
    actualMinutesInOven |> should equal 15
    actualMinutesInOven <- 16
    actualMinutesInOven |> should equal 16
