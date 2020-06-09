In this exercise you're a big sports fan and you've just discovered a passion for NBA basketball. Being new to NBA basketball, you're doing a deep dive into NBA history, keeping track of teams, coaches, their win/loss stats and comparing them against each other.

As you don't yet have a favorite team, you'll also be developing an algorithm to figure out whether to root for a particular team.

You have seven tasks to help you develop your proprietary _root-for-a-team_ algorithm.

### 1. Define the model

Define the `Coach` record with the following two fields:

- `Name`: the coach's name, of type `string`.
- `FormerPlayer`: indicates if the coach was a former player, of type `bool`.

Define the `Stats` record with the following two fields:

- `Wins`: the number of wins, of type `int`.
- `Losses`: the number of losses, of type `int`.

Define the `Team` record with the following three fields:

- `Name`: the team's name, of type `string`.
- `Coach`: the team's coach, of type `Coach`.
- `Stats`: the team's stats, of type `Stats`.

### 2. Create a team's coach

Implement the `createCoach` function that takes the coach name and its former player status as parameters, and returns its `Coach` record:

```fsharp
createCoach "Larry Bird" true
// => { Name = "Larry Bird"; FormerPlayer = true }
```

### 3. Create a team's stats

Implement the `createStats` function that takes the number of wins and the number losses as parameters, and returns its `Stats` record:

```fsharp
createStats 58 24
// => { Wins = 58; Losses = 24 }
```

### 4. Create a team

Implement the `createTeam` function that takes the team name, coach and record as parameters, and returns its `Team` record:

```fsharp
let coach = createCoach "Larry Bird" true
let record = createStats 58 24
createTeam "Indiana Pacers" coach record
// => { Name = "Indiana Pacers"
//      Coach = { Name = "Larry Bird"; FormerPlayer = true }
//      Stats = { Wins = 58; Losses = 24 } }
```

### 5. Replace the coach

NBA owners being impatient, you found that bad team results would often lead to the coach being replaced. Implement the `replaceCoach` function that takes the team and its new coach as parameters, and returns the team but with the new coach:

```fsharp
let coach = createCoach "Larry Bird" true
let record = createStats 58 24
let team = createTeam "Indiana Pacers" coach record

let newCoach = createCoach "Isiah Thomas" true
replaceCoach team newCoach
// => { Name = "Indiana Pacers"
//      Coach = { Name = "Isiah Thomas"; FormerPlayer = true }
//      Stats = { Wins = 58; Losses = 24 } }
```

### 6. Check for same team

While digging into stats, you're keeping lists of teams and their records. Sometimes, you get things wrong and there are duplicate entries on your list. Implement the `isSameTeam` function that takes two teams and returns `true` if they are the same team; otherwise, return `false`:

```fsharp
let pacersCoach = createCoach "Larry Bird" true
let pacersStats = createStats 58 24
let pacersTeam = createTeam "Indiana Pacers" pacersCoach pacersStats

let lakersCoach = createCoach "Del Harris" false
let lakersStats = createStats 61 21
let lakersTeam = createTeam "LA Lakers" lakersCoach lakersStats

isSameTeam pacersTeam lakersTeam
// => false
```

### 7. Check if you should root for a team

Having looked at many teams and matches, you've come up with an algorithm. If one of the following is true, you root for that team:

- The coach's name is "Gregg Popovich"
- The coach is a former player
- The team's name is the "Chicago Bulls"
- The team has won 60 or more games
- The team has more losses than wins

Implement the `rootForTeam` function that takes a team and returns `true` if you should root for that team; otherwise, `return` false:

```fsharp
let spursCoach = createCoach "Gregg Popovich" false
let spursStats = createStats 56 26
let spursTeam = createTeam "San Antonio Spurs" spursCoach spursStats
rootForTeam spursTeam
// => true
```
