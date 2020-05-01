## General

- [Golang map][gobyexample-map]

## 1. Calculate points for each match

- Create a global map to store the points and game result for each team
- Calculate points based on the match score, see instruction to determine the point for each result

## 2. Determine who's the winner of the league

- Loop though the global map
- Return an if the points table is empty

## 3. Implement GetPoints function

- You need to check whether the team is in the your point table or not
- If the asked team is not in point table, return an error
- Otherwise, return the score

[gobyexample-map]: https://gobyexample.com/maps
