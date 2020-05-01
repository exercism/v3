package maps

import (
	"errors"
)

const (
	lose = iota
	draw
	win
)

// pointsTable hold points for each team
var pointsTable map[string]int

// CalculatePoints record the each team score
func CalculatePoints(homeTeam string, awayTeam string, homeScore int, awayScore int) bool {
	if homeScore < 0 || awayScore < 0 {
		return false
	}

	if homeScore > awayScore {
		pointsTable[homeTeam] += win
	} else if homeScore < awayScore {
		pointsTable[awayTeam] += win
	} else if homeScore == awayScore {
		pointsTable[homeTeam] += draw
		pointsTable[awayTeam] += draw
	}

	return true
}

// GetPoints return score for a team based on `team` parameter
func GetPoints(team string) (int, error) {
	if _, ok := pointsTable[team]; !ok {
		return 0, errors.New("Team not found")
	}

	return pointsTable[team], nil
}

// LeagueWinner return the team which win the league
func LeagueWinner() (string, int, error) {
	if len(pointsTable) == 0 {
		return "", 0, errors.New("No data")
	}

	var winner string
	var winnerScore int

	for team, score := range pointsTable {
		if score > winnerScore {
			winner = team
			winnerScore = score
		}
	}

	return winner, winnerScore, nil
}
