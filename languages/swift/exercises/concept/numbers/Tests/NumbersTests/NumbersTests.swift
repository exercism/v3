import XCTest

@testable import Numbers

final class NumbersTests: XCTestCase {
  let runAll = Bool(ProcessInfo.processInfo.environment["RUNALL", default: "false"]) ?? false

  func testdailyRateFrom() {
    XCTAssertEqual(dailyRateFrom(hourlyRate: 60), 480.0, accuracy: 0.001)
  }

  func testmonthlyRoundDown() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(monthlyRateFrom(hourlyRate: 77, withDiscount: 10.5), 12129, accuracy: 0.001)
  }

  func testmonthlyRoundUp() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(monthlyRateFrom(hourlyRate: 80, withDiscount: 10.5), 12602, accuracy: 0.001)
  }

  func testworkdaysIn() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(
      workdaysIn(budget: 20_000, hourlyRate: 80, withDiscount: 11), 35.0, accuracy: 0.001)
  }

  func testworkdaysShouldRoundDown() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(
      workdaysIn(budget: 25_000, hourlyRate: 80, withDiscount: 11), 43, accuracy: 0.001)
  }

  static var allTests = [
    ("testdailyRateFrom", testdailyRateFrom),
    ("testmonthlyRoundDown", testmonthlyRoundDown),
    ("testmonthlyRoundUp", testmonthlyRoundUp),
    ("testworkdaysIn", testworkdaysIn),
    ("testworkdaysShouldRoundDown", testworkdaysShouldRoundDown),
  ]
}
