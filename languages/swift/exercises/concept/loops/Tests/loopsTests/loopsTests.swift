import XCTest

@testable import loops

final class loopsTests: XCTestCase {
  let runAll = true
  //    Bool(ProcessInfo.processInfo.environment["RUNALL", default: "false"])
  //    ?? false

  let checkTest: ((Int, Int), (Int, Int)) -> Bool = { (expected, got) in
    expected.0 == got.0 && expected.1 == got.1
  }
  func testHashIDs() {
    let hashInputs = [1_924_373_548, 1_727_322_459, 2_092_564_539, 3_768_022_803]
    let expected = [
      (1_924_373_548, 3_656_168_320), (1_727_322_459, 1_284_244_920),
      (2_092_564_539, 2_466_365_865), (3_768_022_803, 3_664_596_897),
    ]
    XCTAssertTrue(zip(expected, hashIDs(hashInputs)).allSatisfy(checkTest))
  }

  func testHashIDsEmpty() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertTrue(hashIDs([]).isEmpty)
  }

  func testRankingLevel() throws {
    let rankInputs = [3_656_168_320, 1_284_244_920, 2_466_365_865, 3_664_596_897]
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(rankInputs.map(rankingLevel), [3, 2, 1, 0])
  }

  func testRankIDs() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let hashedInputs = [
      (1_924_373_548, 3_656_168_320), (1_727_322_459, 1_284_244_920),
      (2_092_564_539, 2_466_365_865), (3_768_022_803, 3_664_596_897),
    ]
    let expected = [(1_924_373_548, 3), (1_727_322_459, 2), (2_092_564_539, 1), (3_768_022_803, 0)]
    XCTAssertTrue(zip(expected, rankIDs(hashedIDs: hashedInputs)).allSatisfy(checkTest))
  }

  func testRankIDsEmpty() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertTrue(rankIDs(hashedIDs: []).isEmpty)
  }

  static var allTests = [
    ("testHashIDs", testHashIDs),
    ("testHashIDsEmpty", testHashIDsEmpty),
    ("testRankingLevel", testRankingLevel),
    ("testRankIDs", testRankIDs),
    ("testRankIDsEmpty", testRankIDsEmpty),
  ]
}
