func newScoreBoard() -> [String: Int] {
  [String: Int]()
}

func addPlayer(_ scores: inout [String: Int], _ name: String, _ score: Int = 0) {
  scores[name] = score
}

func removePlayer(_ scores: inout [String: Int], _ name: String) {
  scores.removeValue(forKey: name)
}

func resetScore(_ scores: inout [String: Int], _ name: String) {
  if let _ = scores[name] {
    scores[name] = 0
  }
}

func updateScore(_ scores: inout [String: Int], _ name: String, _ delta: Int) {
  if let score = scores[name] {
    scores[name] = score + delta
  }
}

func orderByPlayers(_ scores: [String: Int]) -> [(String, Int)] {
  func ascendingKeys(_ a: (String, Int), _ b: (String, Int)) -> Bool {
    a.0 < b.0
  }
  return scores.sorted(by: ascendingKeys)
}

func orderByScores(_ scores: [String: Int]) -> [(String, Int)] {
  func descendingValues(_ a: (String, Int), _ b: (String, Int)) -> Bool {
    a.1 > b.1
  }
  return scores.sorted(by: descendingValues)
}
