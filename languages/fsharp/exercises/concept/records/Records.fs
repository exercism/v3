module Records

type Coach =
    { Name: string
      FormerPlayer: bool }

type Record =
    { Wins: int
      Losses: int }

type Team =
    { Name: string
      Coach: Coach
      Record: Record }

let createCoach name formerPlayer = { Name = name; FormerPlayer = formerPlayer }

let createRecord wins losses = { Wins = wins; Losses = losses }

let createTeam name coach record = { Name = name; Coach = coach; Record = record }

let replaceCoach team coach = { team with Coach = coach }

let haveSameRecord homeTeam awayTeam = homeTeam.Record = awayTeam.Record 

let rootForTeam team =
    match team with
    | { Coach = { Name = "Gregg Popovich" } } -> true
    | { Coach = { FormerPlayer = true } } -> true
    | { Name = "Chicago Bulls" } -> true
    | { Record = { Wins = wins; Losses = losses } } -> wins >= 60 || losses >= 60
