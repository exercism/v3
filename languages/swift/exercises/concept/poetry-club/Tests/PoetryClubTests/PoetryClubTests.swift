import XCTest

@testable import PoetryClub

final class PoetryClubTests: XCTestCase {
  let runAll =
    Bool(ProcessInfo.processInfo.environment["RUNALL", default: "false"])
    ?? false

  func testSplitNewlines() {
    XCTAssertEqual(
      splitOnNewlines("Winken.\nBlinken\n\nAnd Nod."),
      ["Winken.", "Blinken", "", "And Nod."]
    )
  }

  func testSplitNoNewlines() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(splitOnNewlines("Hello."), ["Hello."])
  }

  func testFirstLetter() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(firstLetter("Lorem ipsum"), "L")
  }

  func testFirstLetterEmpty() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(firstLetter(""), "_")
  }

  func testCapitalize() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(capitalize("HORSES for CoUrSeS!"), "Horses For Courses!")
  }

  func testTrimWithWhitespace() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(
      trimFromEnd("Is all the whitespace gone?   \t  \t"),
      "Is all the whitespace gone?"
    )
  }

  func testTrimWithoutWhitespace() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(
      trimFromEnd("Is all the whitespace gone?"),
      "Is all the whitespace gone?"
    )
  }

  func testLastLetter() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(lastLetter("Lorem ipsum"), "m")
  }

  func testLastLetterEmpty() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(lastLetter(""), "_")
  }

  func testBackdoorPassword() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(backDoorPassword("scoobyDOO!"), "Scoobydoo!, please")
  }

  func testIthLetter() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(ithLetter("Inquisitive", i: 2), "q")
  }

  func testIthLetterInvalid() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(ithLetter("Inquisitive", i: 100), " ")
  }

  func testSecretRoomPassword() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(secretRoomPassword("Open Sesame"), "OPEN SESAME!")
  }

  static var allTests = [
    ("testSplitNewlines", testSplitNewlines),
    ("testSplitNoNewlines", testSplitNoNewlines),
    ("testFirstLetter", testFirstLetter),
    ("testFirstLetterEmpty", testFirstLetterEmpty),
    ("testCapitalize", testCapitalize),
    ("testTrimWithWhitespace", testTrimWithWhitespace),
    ("testTrimWithoutWhitespace", testTrimWithoutWhitespace),
    ("testLastLetter", testLastLetter),
    ("testLastLetterEmpty", testLastLetterEmpty),
    ("testBackdoorPassword", testBackdoorPassword),
    ("testIthLetter", testIthLetter),
    ("testIthLetterInvalid", testIthLetterInvalid),
    ("testSecretRoomPassword", testSecretRoomPassword),
  ]
}
