package maps

var result = map[string]int{
	"draw": 1,
	"win":  1,
}

// pointsTable hold points for each team
var pointsTable map[string]int

// CalculatePoints record the each team score
func CalculatePoints(homeTeam string, awayTeam string, homeScore int, awayScore int) bool {
	if homeScore < 0 || awayScore < 0 {
		return false
	}

	if homeScore > awayScore {
		pointsTable[homeTeam] += result["win"]
	} else if homeScore < awayScore {
		pointsTable[awayTeam] += result["win"]
	} else if homeScore == awayScore {
		pointsTable[homeTeam] += result["draw"]
		pointsTable[awayTeam] += result["draw"]
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
