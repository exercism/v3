import XCTest

@testable import HighScoreBoard

final class HighScoreBoardTests: XCTestCase {
  let runAll = Bool(ProcessInfo.processInfo.environment["RUNALL", default: "false"]) ?? false

  func testEmptyScores() {
    XCTAssertEqual(newScoreBoard(), [String: Int]())
  }

  func testAddPlayerExplicit() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    var scoreboard = [String: Int]()
    let score = 1337
    addPlayer(&scoreboard, "Jesse Johnson", score)
    if let jjScore = scoreboard["Jesse Johnson"] {
      XCTAssertEqual(
        jjScore,
        score,
        "Called 'addPlayer(&scoreboard, \"Jesse Johnson\", 1337)'\nscoreboard[\"Jesse Johnson\"] expected to be \(score), got \(jjScore) instead"
      )
    } else {
      XCTFail("addPlayer failed to add player to dictionary")
    }
  }

  func testAddPlayerDefault() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    var scoreboard = [String: Int]()
    addPlayer(&scoreboard, "Jesse Johnson")
    if let jjScore = scoreboard["Jesse Johnson"] {
      XCTAssertEqual(
        jjScore,
        0,
        "Called 'addPlayer(&scoreboard, \"Jesse Johnson\"'\nscoreboard[\"Jesse Johnson\"] expected to be \(0), got \(jjScore) instead"
      )
    } else {
      XCTFail("addPlayer failed to add player to dictionary")
    }
  }

  func testRemovePlayer() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    var scoreboard = [String: Int]()
    addPlayer(&scoreboard, "Jesse Johnson", 1337)
    addPlayer(&scoreboard, "Amil PAstorius", 99373)
    addPlayer(&scoreboard, "Min-seo Shin")
    removePlayer(&scoreboard, "Jesse Johnson")
    XCTAssertNil(
      scoreboard["Jesse Johnson"],
      "Removed player \"Jesse Johnson\", \nscoreboard[\"Jesse Johnson\"] expected to be nil, got \(scoreboard["Jesse Johnson"]!) instead"
    )
  }

  func testRemoveNonexistentPlayer() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    var scoreboard = [String: Int]()
    addPlayer(&scoreboard, "Jesse Johnson", 1337)
    addPlayer(&scoreboard, "Amil PAstorius", 99373)
    addPlayer(&scoreboard, "Min-seo Shin")
    removePlayer(&scoreboard, "Bruno Santangelo")
    XCTAssertEqual(
      scoreboard,
      ["Jesse Johnson": 1337, "Amil PAstorius": 99373, "Min-seo Shin": 0],
      "Removing a non-existent player from the dictionary should leave the dictionary unchanged."
    )
  }

  func testResetScore() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    var scoreboard = [String: Int]()
    addPlayer(&scoreboard, "Jesse Johnson", 1337)
    addPlayer(&scoreboard, "Amil PAstorius", 99373)
    addPlayer(&scoreboard, "Min-seo Shin")
    resetScore(&scoreboard, "Amil PAstorius")
    XCTAssertEqual(
      scoreboard,
      ["Jesse Johnson": 1337, "Amil PAstorius": 0, "Min-seo Shin": 0],
      "Resetting a player's score should change their score to 0 and leave the rest unchanged."
    )
  }

  func testUpdateScore() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    var scoreboard = [String: Int]()
    addPlayer(&scoreboard, "Jesse Johnson", 1337)
    addPlayer(&scoreboard, "Amil PAstorius", 99373)
    addPlayer(&scoreboard, "Min-seo Shin")
    updateScore(&scoreboard, "Min-seo Shin", 1999)
    updateScore(&scoreboard, "Jesse Johnson", 1337)
    XCTAssertEqual(
      scoreboard,
      ["Jesse Johnson": 2674, "Amil PAstorius": 99373, "Min-seo Shin": 1999],
      "Updating a player's score should add the update to their score and leave the rest unchanged."
    )
  }

  func testOrderByPlayers() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    var scoreboard = [String: Int]()
    addPlayer(&scoreboard, "Jesse Johnson", 1337)
    addPlayer(&scoreboard, "Amil PAstorius", 99373)
    addPlayer(&scoreboard, "Min-seo Shin")
    updateScore(&scoreboard, "Min-seo Shin", 1999)
    updateScore(&scoreboard, "Jesse Johnson", 1337)
    let expected = [("Amil PAstorius", 99373), ("Jesse Johnson", 2674), ("Min-seo Shin", 1999)]
    let got = orderByPlayers(scoreboard)
    XCTAssertTrue(
      expected.map(\.0) == got.map(\.0) && expected.map(\.1) == got.map(\.1),
      "Expected: \(expected)\nGot: \(got)\norderByPlayers should return the key/value pairs odered descending by the player's name."
    )
  }

  func testOrderByScores() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    var scoreboard = [String: Int]()
    addPlayer(&scoreboard, "Jesse Johnson", 1337)
    addPlayer(&scoreboard, "Amil PAstorius", 99373)
    addPlayer(&scoreboard, "Min-seo Shin")
    updateScore(&scoreboard, "Min-seo Shin", 1999)
    let expected = [("Amil PAstorius", 99373), ("Min-seo Shin", 1999), ("Jesse Johnson", 1337)]
    let got = orderByScores(scoreboard)
    XCTAssertTrue(
      expected.map(\.0) == got.map(\.0) && expected.map(\.1) == got.map(\.1),
      "Expected: \(expected)\nGot: \(got)\norderByPlayers should return the key/value pairs odered ascending by the player's score."
    )
  }

  static var allTests = [
    ("testEmptyScores", testEmptyScores),
    ("testAddPlayerExplicit", testAddPlayerExplicit),
    ("testAddPlayerDefault", testAddPlayerDefault),
    ("testRemovePlayer", testRemovePlayer),
    ("testRemoveNonexistentPlayer", testRemoveNonexistentPlayer),
    ("testResetScore", testResetScore),
    ("testUpdateScore", testUpdateScore),
    ("testOrderByPlayers", testOrderByPlayers),
    ("testOrderByScores", testOrderByScores),
  ]
}
