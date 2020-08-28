import XCTest

@testable import functions

final class functionsTests: XCTestCase {
  let runAll = Bool(ProcessInfo.processInfo.environment["RUNALL", default: "false"]) ?? false

  func testRemainingMinutesExplicit() {
    XCTAssertEqual(remainingMinutesInOven(elapsedMinutes: 22, expectedMinutesInOven: 100), 78)
  }

  func testRemainingMinutesDefault() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(remainingMinutesInOven(elapsedMinutes: 22), 18)
  }

  func testPreparationTime() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(
      preparationTimeInMinutes(
        layers: "sauce", "noodles", "béchamel", "meat", "mozzarella", "noodles", "ricotta",
        "eggplant", "béchamel", "noodles", "sauce", "mozzarella"), 24)
  }

  func testPreparationTimeEmpty() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(preparationTimeInMinutes(), 0)
  }

  func testQuantities() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let amount = quantities(
      layers: "sauce", "noodles", "béchamel", "meat", "mozzarella", "noodles", "ricotta",
      "eggplant", "béchamel", "noodles", "sauce", "mozzarella")
    XCTAssertTrue(
      amount.noodles == 9 && amount.sauce == 0.4, "expected (noodles: 9, sauce: 0.4, got \(amount)")
  }

  func testQuantitiesNoSauce() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let amount = quantities(
      layers: "noodles", "béchamel", "noodles", "ricotta", "eggplant", "mozzarella")
    XCTAssertTrue(
      amount.noodles == 6 && amount.sauce == 0, "expected (noodles: 6, sauce: 0, got \(amount)")
  }

  func testQuantitiesNoNoodles() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let amount = quantities(
      layers: "sauce", "meat", "mozzarella", "eggplant", "béchamel", "sauce", "mozzarella")
    XCTAssertTrue(
      amount.noodles == 0 && amount.sauce == 0.4, "expected (noodles: 0, sauce: 0.4, got \(amount)")
  }

  func testToOz() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    var amount = quantities(
      layers: "sauce", "noodles", "béchamel", "meat", "sauce", "noodles", "sauce", "mozzarella")
    toOz(&amount)
    XCTAssertEqual(amount.sauce, 20.2884, accuracy: 0.001)
  }

  func testRedWineRed() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertTrue(
      redWine(layers: "sauce", "noodles", "sauce", "noodles", "meat", "noodles", "mozzarella"))
  }

  func testRedWineWhite() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertFalse(
      redWine(
        layers: "sauce", "noodles", "béchamel", "meat", "mozzarella", "noodles", "sauce", "ricotta",
        "eggplant", "béchamel", "noodles", "meat", "sauce", "mozzarella"))
  }

  static var allTests = [
    ("testRemainingMinutesExplicit", testRemainingMinutesExplicit),
    ("testRemainingMinutesDefault", testRemainingMinutesDefault),
    ("testPreparationTime", testPreparationTime),
    ("testPreparationTimeEmpty", testPreparationTimeEmpty),
    ("testQuantities", testQuantities),
    ("testQuantitiesNoSauce", testQuantitiesNoSauce),
    ("testQuantitiesNoNoodles", testQuantitiesNoNoodles),
    ("testToOz", testToOz),
    ("testRedWineRed", testRedWineRed),
    ("testRedWineWhite", testRedWineWhite),
  ]
}
