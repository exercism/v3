import XCTest

@testable import tuples

final class tuplesTests: XCTestCase {
  private let runAll =
    Bool(ProcessInfo.processInfo.environment["RUNALL", default: "false"]) ?? false
  private let aboutEqual = { abs($0 - $1) < 0.001 }

  func testCartesianToPolar() {
    let coordinate = (x: 11.713, y: 5.6405)
    let (actualR, actualPhi) = cartesianToPolar(coordinate)
    XCTAssertTrue(aboutEqual(actualR, 13.0) && aboutEqual(actualPhi, 0.4488))
  }

  func testCartesianToPolarQ3() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let coordinate = (x: -4.7292, y: -2.4096)
    let (actualR, actualPhi) = cartesianToPolar(coordinate)
    XCTAssertTrue(aboutEqual(actualR, 5.3077) && aboutEqual(actualPhi, -2.6704))
  }

  func testCombineRecords() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let id = Int.random(in: 0..<1_000_000)
    let toy =
      ["Chemistry set", "Race car", "Box kite", "Building brick set", "Talking doll"]
      .randomElement()!
    let productLead =
      ["Binkles", "Snowii", "Jinkies", "Pippi", "Zippit", "Xandi", "Alf"]
      .randomElement()!
    let children =
      [
        "Inés", "Maxime", "Bandile", "Shaurya", "Екатерина", "Anika", "Yael",
        "Dimitrios", "Евгений", "Iminathi", "Valeria", "Marlon", "Mitsuki",
        "Bjarni", "Venla", "Anton", "Beatriz", "Joo-won", "Zahra", "Liam", "Parker",
      ]
    let numberOfRecipients = Int.random(in: 0..<children.count)
    let recipients = Array(children.shuffled().prefix(numberOfRecipients))
    let actual =
      combineRecords(
        production: (toy: toy, id: id, productLead: productLead),
        gifts: (id, recipients)
      )
    XCTAssertTrue(
      actual.id == id && actual.toy == toy && actual.productLead == productLead
        && actual.recipients == recipients
    )
  }
  static var allTests = [
    ("testCartesianToPolar", testCaretesianToPolar),
    ("testCartesianToPolarQ3", testCaretesianToPolarQ3),
    ("testCombineRecords", testCombineRecords),
  ]
}
