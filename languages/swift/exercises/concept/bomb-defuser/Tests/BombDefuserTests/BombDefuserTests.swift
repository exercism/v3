import XCTest

@testable import BombDefuser

final class BombDefuserTests: XCTestCase {
  let runAll = Bool(ProcessInfo.processInfo.environment["RUNALL", default: "false"]) ?? false
  let stringify = { (tuple: (String, String, String)) in "\(tuple)" }

  func testFlip() {
    let expected = ("Dabba", "Yabba", "Doo")
    let got = flip(("Yabba", "Dabba", "Doo"))
    XCTAssertEqual(
      stringify(expected), stringify(got),
      "flip((\"Yabba\", \"Dabba\", \"Doo\"): Expected \(expected), got \(got)")
  }

  func testRotate() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let expected = ("Dooby", "Doo", "Scooby")
    let got = rotate(("Scooby", "Dooby", "Doo"))
    XCTAssertEqual(
      stringify(expected), stringify(got),
      "rotate((\"Scooby\", \"Dooby\", \"Doo\"): Expected \(expected), got \(got)")
  }

  func testShuffle1() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let wires = ("Red", "Yellow", "Black")
    let shuffle = makeShuffle(flipper: flip, rotator: rotate)
    let expected = ("Yellow", "Black", "Red")
    let got = shuffle(113, wires)
    XCTAssertEqual(
      stringify(expected), stringify(got),
      "shuffle(113, (\"Red\", \"Yellow\", \"Black\")): Expected \(expected), got \(got)")
  }

  func testShuffle2() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let wires = ("Purple", "Cyan", "Marigold")
    let shuffle = makeShuffle(flipper: flip, rotator: rotate)
    let expected = ("Marigold", "Cyan", "Purple")
    let got = shuffle(253, wires)
    XCTAssertEqual(
      stringify(expected), stringify(got),
      "shuffle(253, (\"Purple\", \"Cyan\", \"Marigold\")): Expected \(expected), got \(got)")
  }

  func testShuffle3() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let wires = ("Brown", "Orange", "White")
    let shuffle = makeShuffle(flipper: flip, rotator: rotate)
    let expected = ("Brown", "Orange", "White")
    let got = shuffle(0, wires)
    XCTAssertEqual(
      stringify(expected), stringify(got),
      "shuffle(0, (\"Brown\", \"Orange\", \"White\")): Expected \(expected), got \(got)")
  }

  static var allTests = [
    ("testFlip", testFlip),
    ("testRotate", testRotate),
    ("testShuffle1", testShuffle1),
    ("testShuffle2", testShuffle2),
    ("testShuffle3", testShuffle3),
  ]
}
