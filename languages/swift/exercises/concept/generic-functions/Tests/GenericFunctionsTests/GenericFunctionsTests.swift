import XCTest

@testable import GenericFunctions

final class GenericFunctionsTests: XCTestCase {
  func testExample() {
    // This is an example of a functional test case.
    // Use XCTAssert and related functions to verify your tests produce the correct
    // results.
    XCTAssertEqual(GenericFunctions().text, "Hello, World!")
  }

  static var allTests = [
    ("testExample", testExample)
  ]
}
