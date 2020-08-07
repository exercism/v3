import XCTest
@testable import HigherOrderFunctions

final class HigherOrderFunctionsTests: XCTestCase {
    func testExample() {
        // This is an example of a functional test case.
        // Use XCTAssert and related functions to verify your tests produce the correct
        // results.
        XCTAssertEqual(HigherOrderFunctions().text, "Hello, World!")
    }

    static var allTests = [
        ("testExample", testExample),
    ]
}
