import XCTest

@testable import PizzaSlices

final class PizzaSlicesTests: XCTestCase {
  let runAll = Bool(ProcessInfo.processInfo.environment["RUNALL", default: "false"]) ?? false

  func testSliceNormal() throws {
    let size = try XCTUnwrap(sliceSize(diameter: 16, slices: 10))
    XCTAssertEqual(size, Double.pi * 6.4, accuracy: 0.01)
  }

  func testSliceNilDiameter() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertNil(sliceSize(diameter: nil, slices: 10))
  }

  func testSliceNilSlices() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertNil(sliceSize(diameter: 16, slices: nil))
  }

  func testSliceBadDiameter() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertNil(sliceSize(diameter: -16, slices: 10))
  }

  func testSliceBadSlices() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertNil(sliceSize(diameter: 16, slices: 0))
  }

  func testABiggest() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let biggest = biggestSlice(diameterA: "16", slicesA: "8", diameterB: "12", slicesB: "6")
    XCTAssertEqual(biggest, "Slice A is bigger")
  }

  func testBBiggest() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let biggest = biggestSlice(diameterA: "16", slicesA: "10", diameterB: "18", slicesB: "12")
    XCTAssertEqual(biggest, "Slice B is bigger")
  }

  func testBothSame() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let biggest = biggestSlice(diameterA: "16", slicesA: "10", diameterB: "16", slicesB: "10")
    XCTAssertEqual(biggest, "Neither slice is bigger")
  }

  func testANil() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let biggest = biggestSlice(diameterA: "-16", slicesA: "8", diameterB: "12", slicesB: "6")
    XCTAssertEqual(biggest, "Slice B is bigger")
  }

  func testBNil() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let biggest = biggestSlice(diameterA: "16", slicesA: "8", diameterB: "-18", slicesB: "12")
    XCTAssertEqual(biggest, "Slice A is bigger")
  }

  func testBothNil() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let biggest = biggestSlice(diameterA: "16", slicesA: "-8", diameterB: "16 inches", slicesB: "8")
    XCTAssertEqual(biggest, "Neither slice is bigger")
  }

  func testZeroIsValid() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let biggest = biggestSlice(diameterA: "0", slicesA: "8", diameterB: "16 inches", slicesB: "8")
    XCTAssertEqual(biggest, "Slice A is bigger")
  }

  static var allTests = [
    ("testSliceNormal", testSliceNormal),
    ("testSliceNilDiameter", testSliceNilDiameter),
    ("testSliceNilSlices", testSliceNilSlices),
    ("testSliceBadDiameter", testSliceBadDiameter),
    ("testSliceBadSlices", testSliceBadSlices),
    ("testABiggest", testABiggest),
    ("testBBiggest", testBBiggest),
    ("testBothSame", testBothSame),
    ("testANil", testANil),
    ("testBNil", testBNil),
    ("testBothNil", testBothNil),
    ("testZeroIsValid", testZeroIsValid),
  ]
}
