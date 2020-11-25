module Stats

type Coach = { Name: string; FormerPlayer: bool }

type Stats = { Wins: int; Losses: int }

type Team =
    { Name: string
      Coach: Coach
      Stats: Stats }

let createCoach name formerPlayer =
    { Name = name
      FormerPlayer = formerPlayer }

let createStats wins losses = { Wins = wins; Losses = losses }

let createTeam name coach stats =
    { Name = name
      Coach = coach
      Stats = stats }

let replaceCoach team coach = { team with Coach = coach }

let isSameTeam homeTeam awayTeam = homeTeam = awayTeam

let rootForTeam team =
    match team with
    | { Coach = { Name = "Gregg Popovich" } } -> true
    | { Coach = { FormerPlayer = true } } -> true
    | { Name = "Chicago Bulls" } -> true
    | { Stats = { Wins = wins; Losses = losses } } -> wins >= 60 || losses > wins
