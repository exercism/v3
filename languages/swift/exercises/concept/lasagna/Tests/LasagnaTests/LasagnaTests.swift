import XCTest
@testable import Lasagna

final class LasagnaTests: XCTestCase {
  let runAll = Bool(ProcessInfo.processInfo.environment["RUNALL", default: "false"]) ?? false

  func testExpectedMinutes() {
    XCTAssertEqual(expectedMinutesInOven, 40)
  }

  func testRemainingMinutes() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(remainingMinutesInOven(elapsedMinutes: 13), 27)
  }

  func testPreperationMinutes() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(preparationTimeInMinutes(layers: 6), 12)
  }

  func testTotalMinutes() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(totalTimeInMinutes(layers: 6, elapsedMinutes: 13), 39)
  }

  static var allTests = [
    ("testExpectedMinutes", testExpectedMinutes),
    ("testRemainingMinutes", testRemainingMinutes),
    ("testPreperationMinutes", testPreperationMinutes),
    ("testTotalMinutes", testTotalMinutes),
  ]
}
