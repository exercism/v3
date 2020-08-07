import XCTest

@testable import stringsAndCharacters

final class stringsAndCharactersTests: XCTestCase {
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

  func testNumberOfLines() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(numberOfLines(in: graduationFor(name: "Isabel", year: 1988)), 2)
  }

  func testNumberOfLines2() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(numberOfLines(in: "Hello\n\n\n\n\n\n\n\n\n\nWorld\n"), 12)
  }

  func testNumberOfLines3() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(numberOfLines(in: ""), 1)
  }

  func testNumberofNumbers() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(numberOfNumbers(in: graduationFor(name: "Isabel", year: 1988)), 4)
  }

  func testNumberofNumbers2() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(numberOfNumbers(in: "No numbers here"), 0)
  }

  func testNumberofNumbers3() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(numberOfNumbers(in: "N0 numb3r5 ½3r3!"), 6)
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
    ("testNumberOfLines", testNumberOfLines),
    ("testNumberOfLines2", testNumberOfLines2),
    ("testNumberOfLines3", testNumberOfLines3),
    ("testNumberofNumbers", testNumberofNumbers),
    ("testNumberofNumbers2", testNumberofNumbers2),
    ("testNumberofNumbers3", testNumberofNumbers3),
  ]
}
