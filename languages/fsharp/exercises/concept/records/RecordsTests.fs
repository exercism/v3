module RecordsTests

open FsUnit.Xunit
open Xunit

open Records

[<Fact>]
let ``Create coach that was a former player`` () =
    createCoach "Steve Kerr" true
    |> should equal
           { Name = "Steve Kerr"
             FormerPlayer = true }

[<Fact>]
let ``Create coach that wasn't a former player`` () =
    createCoach "Erik Spoelstra" false
    |> should equal
           { Name = "Erik Spoelstra"
             FormerPlayer = false }

[<Fact>]
let ``Create stats for winning team`` () =
    createStats 55 27
    |> should equal { Wins = 55; Losses = 27 }

[<Fact>]
let ``Create stats for losing team`` () =
    createStats 39 43
    |> should equal { Wins = 39; Losses = 43 }

[<Fact>]
let ``Create stats for all-time season record`` () =
    createStats 73 9
    |> should equal { Wins = 73; Losses = 9 }
