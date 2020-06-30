import XCTest

@testable import tuples

final class tuplesTests: XCTestCase {
  private let runAll =
    Bool(ProcessInfo.processInfo.environment["RUNALL", default: "false"]) ?? false
  private let aboutEqual = { abs($0 - $1) < 0.001 }

  func testCaretesianToPolar() {
    let coordinate = (x: 11.713, y: 5.6405)
    let (actualR, actualTheta) = cartesianToPolar(coordinate)
    XCTAssertTrue(aboutEqual(actualR, 13.0) && aboutEqual(actualTheta, 0.4488))
  }

  func testCaretesianToPolarQ3() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let coordinate = (x: -4.7292, y: -2.4096)
    let (actualR, actualTheta) = cartesianToPolar(coordinate)
    XCTAssertTrue(aboutEqual(actualR, 5.3077) && aboutEqual(actualTheta, -2.6704))
  }

  func testcombineRecords() throws {
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

  //    func testPreperationMinutes() throws {
  //        try XCTSkipIf(true && !runAll) // change true to false to run this test
  //        XCTAssertEqual(preparationTimeInMinutes(layers: 6), 12)
  //    }

  static var allTests = [
    ("testCaretesianToPolar", testCaretesianToPolar),
    ("testCaretesianToPolarQ3", testCaretesianToPolarQ3),
    ("testcombineRecords", testcombineRecords),
  ]
}
