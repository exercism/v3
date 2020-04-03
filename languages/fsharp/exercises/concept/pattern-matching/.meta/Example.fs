module PatternMatching

let reply guess =
    match guess with
    | 41 -> "So close"
    | 43 -> "So close"
    | _ when guess < 41 -> "Too low"
    | _ when guess > 43 -> "Too high"
    | _ -> "Correct"
