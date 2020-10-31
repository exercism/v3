import XCTest

@testable import MasterMixologist

typealias DrinkTrack = (first: String, last: String, total: Int)?

final class MasterMixologistTests: XCTestCase {
  let runAll =
    Bool(ProcessInfo.processInfo.environment["RUNALL", default: "false"]) ?? false

  func checkTest(e: DrinkTrack, g: DrinkTrack) -> Bool {
    guard let expected = e, let got = g else { return e == nil && g == nil }
    return expected.first == got.first && expected.last == got.last && expected.total == got.total
  }

  let orders = [
    ["beer"], ["water"], ["soda"], ["shot"], ["mixed drink"], ["fancy drink"], ["frozen drink"],
    ["beer", "shot", "fancy drink"],
    ["beer", "shot", "water", "fancy drink", "frozen drink", "fancy drink"],
    ["mixed drink", "water", "soda", "soda", "beer"], Array(repeating: "frozen drink", count: 10),
  ]

  func testTimeToPrepare() {
    XCTAssertEqual([0.5, 0.5, 0.5, 1, 1.5, 2.5, 3, 4, 10, 3.5, 30], orders.map(timeToPrepare))
  }

  func testMakeWedges() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let got = makeWedges(
      needed: 42, limes: ["small", "large", "large", "medium", "small", "large", "large"])
    XCTAssertEqual(got, 6, "You needto use 6 limes to fill the bin; you said you need \(got).")
  }

  func testMakeWedgesNoNeed() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let got = makeWedges(
      needed: 0, limes: ["small", "large", "large", "medium", "small", "large", "large"])
    XCTAssertEqual(got, 0, "Your bin was full, so you used 0 limes; you said you used \(got).")
  }

  func testMakeWedgesNoLimes() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let got = makeWedges(needed: 42, limes: [])
    XCTAssertEqual(
      got, 0, "You have no limes to cut up so you used 0 limes; you said you used \(got).")
  }

  func testMakeWedgesTooFewLimes() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let got = makeWedges(
      needed: 80, limes: ["small", "large", "large", "medium", "small", "large", "large"])
    XCTAssertEqual(
      got, 7, "You used 7 limes before you ran out of limes; you said you used \(got).")
  }

  func testFinishShift() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let got = finishShift(minutesLeft: 12, remainingOrders: orders)
    let expected = Array(orders.dropFirst(8))
    XCTAssertEqual(
      got, expected,
      "You were expected to leave the orders \(expected) for the next shift; you left \(got).")
  }

  func testFinishShiftJustRunOver() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let got = finishShift(minutesLeft: 30, remainingOrders: orders)
    let expected: [[String]] = []
    XCTAssertEqual(
      got, expected,
      "You were expected to leave the orders \(expected) for the next shift; you left \(got).")
  }

  func testFinishShiftLeaveEarly() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let got = finishShift(minutesLeft: 120, remainingOrders: orders)
    let expected: [[String]] = []
    XCTAssertEqual(
      got, expected,
      "You were expected to leave the orders \(expected) for the next shift; you left \(got).")
  }

  func testOrderTracker() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let orders = [
      (drink: "beer", time: "10:01"), (drink: "soda", time: "10:02"),
      (drink: "shot", time: "10:05"), (drink: "fancy drink", time: "10:06"),
      (drink: "soda", time: "10:09"), (drink: "beer", time: "10:15"),
      (drink: "beer", time: "10:22"), (drink: "water", time: "10:26"),
      (drink: "beer", time: "10:28"), (drink: "soda", time: "10:33"),
    ]
    let expectedBeers: DrinkTrack = (first: "10:01", last: "10:28", total: 4)
    let expectedSodas: DrinkTrack = (first: "10:02", last: "10:33", total: 3)
    let got = orderTracker(orders: orders)
    XCTAssertTrue(
      checkTest(e: expectedBeers, g: got.beer) && checkTest(e: expectedSodas, g: got.soda),
      "Expected (beer: \(expectedBeers!), soda: \(expectedSodas!)), got: \(got)")
  }

  func testOrderOneEach() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let orders = [
      (drink: "beer", time: "10:01"), (drink: "soda", time: "10:02"),
      (drink: "shot", time: "10:05"), (drink: "fancy drink", time: "10:06"),
      (drink: "water", time: "10:26"),
    ]
    let expectedBeers: DrinkTrack = (first: "10:01", last: "10:01", total: 1)
    let expectedSodas: DrinkTrack = (first: "10:02", last: "10:02", total: 1)
    let got = orderTracker(orders: orders)
    XCTAssertTrue(
      checkTest(e: expectedBeers, g: got.beer) && checkTest(e: expectedSodas, g: got.soda),
      "Expected (beer: \(expectedBeers!), soda: \(expectedSodas!)), got: \(got)")
  }

  func testOrderTrackerNoBeer() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let orders = [
      (drink: "soda", time: "10:02"), (drink: "shot", time: "10:05"),
      (drink: "fancy drink", time: "10:06"), (drink: "soda", time: "10:09"),
      (drink: "water", time: "10:26"), (drink: "soda", time: "10:33"),
    ]
    let expectedBeers: DrinkTrack = nil
    let expectedSodas: DrinkTrack = (first: "10:02", last: "10:33", total: 3)
    let got = orderTracker(orders: orders)
    XCTAssertTrue(
      checkTest(e: expectedBeers, g: got.beer) && checkTest(e: expectedSodas, g: got.soda),
      "Expected (beer: nil, soda: \(expectedSodas!)), got: \(got)")
  }

  func testOrderTrackerNoSoda() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let orders = [
      (drink: "beer", time: "10:01"), (drink: "shot", time: "10:05"),
      (drink: "fancy drink", time: "10:06"), (drink: "beer", time: "10:15"),
      (drink: "beer", time: "10:22"), (drink: "water", time: "10:26"),
      (drink: "beer", time: "10:28"),
    ]
    let expectedBeers: DrinkTrack = (first: "10:01", last: "10:28", total: 4)
    let expectedSodas: DrinkTrack = nil
    let got = orderTracker(orders: orders)
    XCTAssertTrue(
      checkTest(e: expectedBeers, g: got.beer) && checkTest(e: expectedSodas, g: got.soda),
      "Expected (beer: \(expectedBeers!), soda: nil), got: \(got)")
  }

  func testOrderTrackerNils() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let orders = [(drink: String, time: String)]()
    let expectedBeers: DrinkTrack = nil
    let expectedSodas: DrinkTrack = nil
    let got = orderTracker(orders: orders)
    XCTAssertTrue(
      checkTest(e: expectedBeers, g: got.beer) && checkTest(e: expectedSodas, g: got.soda),
      "Expected (beer: nil, soda: nil), got: \(got)")
  }

  static var allTests = [
    ("testTimeToPrepare", testTimeToPrepare),
    ("testMakeWedges", testMakeWedges),
    ("testMakeWedgesNoNeed", testMakeWedgesNoNeed),
    ("testMakeWedgesNoLimes", testMakeWedgesNoLimes),
    ("testMakeWedgesTooFewLimes", testMakeWedgesTooFewLimes),
    ("testFinishShift", testFinishShift),
    ("testFinishShiftJustRunOver", testFinishShiftJustRunOver),
    ("testFinishShiftLeaveEarly", testFinishShiftLeaveEarly),
    ("testOrderTracker", testOrderTracker),
    ("testOrderOneEach", testOrderOneEach),
    ("testOrderTrackerNoBeer", testOrderTrackerNoBeer),
    ("testOrderTrackerNoSoda", testOrderTrackerNoSoda),
    ("testOrderTrackerNils", testOrderTrackerNils),
  ]
}
