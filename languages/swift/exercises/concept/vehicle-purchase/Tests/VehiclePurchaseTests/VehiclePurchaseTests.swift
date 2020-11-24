import XCTest

@testable import VehiclePurchase

final class VehiclePurchaseTests: XCTestCase {
  let runAll = Bool(ProcessInfo.processInfo.environment["RUNALL", default: "false"]) ?? false

  func testCanBuy() {
    XCTAssertEqual(
      canIBuy(vehicle: "1974 Ford Pinto", price: 516.32, monthlyBudget: 100.00),
      "Yes! I'm getting a 1974 Ford Pinto")
  }

  func testCannotBuy() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(
      canIBuy(vehicle: "2011 Bugatti Veryon", price: 2_250_880.00, monthlyBudget: 10000.00),
      "Darn! No 2011 Bugatti Veryon for me")
  }

  func testBeFrugal() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(
      canIBuy(vehicle: "2020 Indian FTR 1200", price: 12_500, monthlyBudget: 200),
      "I'll have to be frugal if I want a 2020 Indian FTR 1200")
  }

  func testTwoWheels() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(
      licenseType(numberOfWheels: 2), "You will need a motorcycle license for your vehicle")
  }

  func testSixWheels() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(
      licenseType(numberOfWheels: 6), "You will need an automobile license for your vehicle")
  }

  func testEighteenWheels() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(
      licenseType(numberOfWheels: 18),
      "You will need a commercial trucking license for your vehicle")
  }

  func testZeroWheels() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(
      licenseType(numberOfWheels: 0), "We do not issue licenses for those types of vehicles")
  }

  func testRegistrationAtNineYears() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(registrationFee(msrp: 2_250_800, yearsOld: 9), 2250)
  }

  func testRegistrationAtZeroYears() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(registrationFee(msrp: 35_000, yearsOld: 0), 350)
  }

  func testRegistrationCheapCar() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(registrationFee(msrp: 5_000, yearsOld: 0), 250)
  }

  func testRegistrationOverTenYears() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(registrationFee(msrp: 34_000, yearsOld: 30), 25)
  }

  static var allTests = [
    ("testCanBuy", testCanBuy),
    ("testCannotBuy", testCannotBuy),
    ("testBeFrugal", testBeFrugal),
    ("testTwoWheels", testTwoWheels),
    ("testSixWheels", testSixWheels),
    ("testEighteenWheels", testEighteenWheels),
    ("testZeroWheels", testZeroWheels),
    ("testRegistrationAtNineYears", testRegistrationAtNineYears),
    ("testRegistrationAtZeroYears", testRegistrationAtZeroYears),
    ("testRegistrationCheapCar", testRegistrationCheapCar),
    ("testRegistrationOverTenYears", testRegistrationOverTenYears),
  ]
}
