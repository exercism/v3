import XCTest

@testable import GetOrganized

final class GetOrganizedTests: XCTestCase {
  let runAll = Bool(ProcessInfo.processInfo.environment["RUNALL", default: "false"]) ?? false

  func testSwapAt() {
    var input = [7, 9, 2, 3, 5, 4, 1, 6, 8, 10]
    swapAt(&input, 5, 7)
    XCTAssertEqual(input, [7, 9, 2, 3, 5, 6, 1, 4, 8, 10])
  }

  func testSwapAtInvald() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    var input = [7, 9, 2, 3, 5, 4, 1, 6, 8, 10]
    swapAt(&input, 5, 17)
    XCTAssertEqual(input, [7, 9, 2, 3, 5, 4, 1, 6, 8, 10])
  }

  func testSortInt() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    var input = [7, 9, 2, 3, 5, 4, 1, 6, 8, 10]
    bubbleSort(&input, swapIf: (<))
    XCTAssertEqual(input, [10, 9, 8, 7, 6, 5, 4, 3, 2, 1])

  }

  func testSortString() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    var input = ["moe", "eenie", "miney", "meenie"]
    bubbleSort(&input, swapIf: (>))
    XCTAssertEqual(input, ["eenie", "meenie", "miney", "moe"])
  }

  static var allTests = [
    ("testSwapAt", testSwapAt),
    ("testSwapAtInvald", testSwapAtInvald),
    ("testSortInt", testSortInt),
    ("testSortString", testSortString),
  ]
}
