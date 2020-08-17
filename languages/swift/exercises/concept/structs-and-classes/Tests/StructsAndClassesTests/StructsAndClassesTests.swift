import XCTest
@testable import StructsAndClasses

final class StructsAndClassesTests: XCTestCase {
    func testExample() {
        // This is an example of a functional test case.
        // Use XCTAssert and related functions to verify your tests produce the correct
        // results.
        XCTAssertEqual(StructsAndClasses().text, "Hello, World!")
    }

    static var allTests = [
        ("testExample", testExample),
    ]
}
