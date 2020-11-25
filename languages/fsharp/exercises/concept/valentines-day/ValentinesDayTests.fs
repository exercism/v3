module ValentinesDayTests

open FsUnit.Xunit
open Xunit

open ValentinesDay

[<Fact>]
let ``Rate board game``() = rateActivity BoardGame |> should equal No

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Rate chilling``() = rateActivity Chill |> should equal No

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Rate crime movie``() = rateActivity (Movie Crime) |> should equal No

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Rate horror movie``() = rateActivity (Movie Horror) |> should equal No

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Rate romance movie``() = rateActivity (Movie Romance) |> should equal Yes

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Rate thriller movie``() = rateActivity (Movie Thriller) |> should equal No

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Rate Korean restaurant``() = rateActivity (Restaurant Korean) |> should equal Yes

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Rate Turkish restaurant``() = rateActivity (Restaurant Turkish) |> should equal Maybe

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Rate walk of 1 kilometer``() = rateActivity (Walk 1) |> should equal Yes

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Rate walk of 2 kilometers``() = rateActivity (Walk 2) |> should equal Yes

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Rate walk of 3 kilometers``() = rateActivity (Walk 3) |> should equal Maybe

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Rate walk of 4 kilometers``() = rateActivity (Walk 4) |> should equal Maybe

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Rate walk of 5 kilometers``() = rateActivity (Walk 5) |> should equal No

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Rate walk over 5 kilometers``() = rateActivity (Walk 8) |> should equal No
