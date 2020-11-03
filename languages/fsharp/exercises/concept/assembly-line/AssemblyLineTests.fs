module NumbersTests

open FsUnit.Xunit
open Xunit

open Numbers

[<Fact>]
let ``Production rate per hour for speed 0``() = productionRatePerHour 0 |> should equal 0.0

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Production rate per hour for speed 1``() = productionRatePerHour 1 |> should equal 221.0

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Production rate per hour for speed 4``() = productionRatePerHour 4 |> should equal 884.0

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Production rate per hour for speed 7``() = productionRatePerHour 7 |> should equal 1392.3

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Production rate per hour for speed 9``() = productionRatePerHour 9 |> should equal 1591.2

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Production rate per hour for speed 10``() = productionRatePerHour 10 |> should equal 1701.7

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Working items per minute for speed 0``() = workingItemsPerMinute 0 |> should equal 0

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Working items per minute for speed 1``() = workingItemsPerMinute 1 |> should equal 3

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Working items per minute for speed 5``() = workingItemsPerMinute 5 |> should equal 16

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Working items per minute for speed 8``() = workingItemsPerMinute 8 |> should equal 26

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Working items per minute for speed 9``() = workingItemsPerMinute 9 |> should equal 26

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Working items per minute for speed 10``() = workingItemsPerMinute 10 |> should equal 28
