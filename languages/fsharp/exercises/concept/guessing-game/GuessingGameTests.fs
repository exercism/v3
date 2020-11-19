module GuessingGameTests

open FsUnit.Xunit
open Xunit

open GuessingGame

[<Fact>]
let ``Give hint for 42``() = reply 42 |> should equal "Correct"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Give hint for 41``() = reply 41 |> should equal "So close"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Give hint for 43``() = reply 43 |> should equal "So close"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Give hint for 40``() = reply 40 |> should equal "Too low"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Give hint for 1``() = reply 1 |> should equal "Too low"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Give hint for 44``() = reply 44 |> should equal "Too high"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Give hint for 100``() = reply 100 |> should equal "Too high"
