import XCTest
@testable import closures

final class closuresTests: XCTestCase {
    func testExample() {
        // This is an example of a functional test case.
        // Use XCTAssert and related functions to verify your tests produce the correct
        // results.
        XCTAssertEqual(closures().text, "Hello, World!")
    }

    static var allTests = [
        ("testExample", testExample),
    ]
}
