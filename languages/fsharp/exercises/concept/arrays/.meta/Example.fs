module Arrays

let lastWeek: int [] = [| 0; 2; 5; 3; 7; 8; 4 |]

let yesterday (counts: int []) = counts.[5]

let total (counts: int []) = Array.sum counts

let dayWithoutBirds (counts: int []) = Array.contains 0 counts

let incrementTodaysCount (counts: int []): int [] =
    counts.[6] <- counts.[6] + 1
    counts

let oddWeek (counts: int []) =
    match counts with
    | [| 1; 0; 1; 0; 1; 0; 1 |] -> true
    | _ -> false
