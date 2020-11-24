import XCTest

@testable import CustomSigns

final class CustomSignsTests: XCTestCase {
  let runAll = Bool(ProcessInfo.processInfo.environment["RUNALL", default: "false"]) ?? false

  func testBirthday() {
    XCTAssertEqual(birthday, "Birthday")
  }

  func testValentine() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(valentine, "Valentine's Day")
  }

  func testAnniversary() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(anniversary, "Anniversary")
  }

  func testSpace() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(space, " " as Character)
  }

  func testExclamation() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(exclamation, "!" as Character)
  }

  func testBuildSign() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(buildSign(for: valentine, name: "Hikaru"), "Happy Valentine's Day Hikaru!")
  }

  func testBuildSign2() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(buildSign(for: birthday, name: ""), "Happy Birthday !")
  }

  func testGraduation() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(
      graduationFor(name: "Isabel", year: 1988), "Congratulations Isabel!\nClass of 1988")
  }

  func testCostOfSign() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(costOf(sign: graduationFor(name: "Isabel", year: 1988)), 94)
  }

  func testCostOfSignEmpty() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(costOf(sign: ""), 20)
  }

  static var allTests = [
    ("testBirthday", testBirthday),
    ("testValentine", testValentine),
    ("testAnniversary", testAnniversary),
    ("testSpace", testSpace),
    ("testExclamation", testExclamation),
    ("testBuildSign", testBuildSign),
    ("testBuildSign2", testBuildSign2),
    ("testGraduation", testGraduation),
    ("testCostOfSign", testCostOfSign),
    ("testCostOfSignEmpty", testCostOfSignEmpty),
  ]
}
