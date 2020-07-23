import XCTest
@testable import stringsAndCharacters

final class stringsAndCharactersTests: XCTestCase {
    func testExample() {
        // This is an example of a functional test case.
        // Use XCTAssert and related functions to verify your tests produce the correct
        // results.
        XCTAssertEqual(stringsAndCharacters().text, "Hello, World!")
    }

    static var allTests = [
        ("testExample", testExample),
    ]
}
