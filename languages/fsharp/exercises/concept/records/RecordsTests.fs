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

[<Fact>]
let ``Create 60's team`` () =
    let coach = createCoach "Red Auerbach" false
    let stats = createStats 58 22
    let team = createTeam "Boston Celtics" coach stats

    team
    |> should equal
           { Name = "Boston Celtics"
             Coach =
                 { Name = "Red Auerbach"
                   FormerPlayer = false }
             Stats = { Wins = 58; Losses = 22 } }

[<Fact>]
let ``Create 2010's team`` () =
    let coach = createCoach "Rick Carlisle" false
    let stats = createStats 57 25

    let team =
        createTeam "Dallas Mavericks" coach stats

    team
    |> should equal
           { Name = "Dallas Mavericks"
             Coach =
                 { Name = "Rick Carlisle"
                   FormerPlayer = true }
             Stats = { Wins = 57; Losses = 25 } }

[<Fact>]
let ``Replace coach mid-season`` () =
    let oldCoach = createCoach "Willis Reed" true
    let newCoach = createCoach "Red Holzman" true
    let stats = createStats 6 81

    let team =
        createTeam "New York Knicks" oldCoach stats

    replaceCoach team newCoach
    |> should equal
           { Name = "New York Knicks"
             Coach =
                 { Name = "Red Holzman"
                   FormerPlayer = true }
             Stats = { Wins = 6; Losses = 8 } }

[<Fact>]
let ``Replace coach after season`` () =
    let oldCoach = createCoach "Rudy Tomjanovich" true
    let newCoach = createCoach "Jeff van Gundy" true
    let stats = createStats 43 39

    let team =
        createTeam "Houston Rockets" oldCoach stats

    replaceCoach team newCoach
    |> should equal
           { Name = "Houston Rockets"
             Coach =
                 { Name = "Jeff van Gundy"
                   FormerPlayer = true }
             Stats = { Wins = 43; Losses = 39 } }
