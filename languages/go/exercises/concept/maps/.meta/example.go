package maps

// gameResult hold points for each game result
var gameResult = map[string]int{
	"draw": 1,
	"win":  2,
}

// pointsTable hold points for each team
var pointsTable map[string]int

// CalculatePoints record the each team score
func CalculatePoints(homeTeam string, awayTeam string, homeScore int, awayScore int) bool {
	if homeScore < 0 || awayScore < 0 {
		return false
	}

	if homeScore > awayScore {
		pointsTable[homeTeam] += gameResult["win"]
	} else if homeScore < awayScore {
		pointsTable[awayTeam] += gameResult["win"]
	} else if homeScore == awayScore {
		pointsTable[homeTeam] += gameResult["draw"]
		pointsTable[awayTeam] += gameResult["draw"]
	}

	return true
}

// GetPoints return score for a team based on `team` parameter
func GetPoints(team string) (int, bool) {
	if _, ok := pointsTable[team]; !ok {
		return 0, false
	}

	return pointsTable[team], false
}
