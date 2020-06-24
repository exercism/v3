module Records

// TODO: please define the 'Coach' discriminated union type

// TODO: please define the 'Stats' discriminated union type

// TODO: please define the 'Team' discriminated union type

let createCoach (name: string) (formerPlayer: bool): Coach =
    failwith "Please implement the 'createCoach' function"

let createStats (wins: int) (losses: int): Stats =
    failwith "Please implement the 'createStats' function"

let createTeam (name: string) (coach: Coach) (stats: Stats): Team =
    failwith "Please implement the 'createTeam' function"

let replaceCoach (team: Team) (coach: Coach): Team =
    failwith "Please implement the 'replaceCoach' function"

let isSameTeam (homeTeam: Team) (awayTeam: Team): bool =
    failwith "Please implement the 'isSameTeam' function"

let rootForTeam (team: Team): bool =
    failwith "Please implement the 'rootForTeam' function"
