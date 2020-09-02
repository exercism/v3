import XCTest

@testable import StructsAndClasses

final class StructsAndClassesTests: XCTestCase {
  let runAll = Bool(ProcessInfo.processInfo.environment["RUNALL", default: "false"]) ?? false

  func testNewWindow() {
    // This is an example of a functional test case.
    // Use XCTAssert and related functions to verify your tests produce the correct
    // results.
    XCTAssertEqual(
      Window().display(),
      "New Window\nPosition: (0, 0), Size: (80 x 60)\n[This window intentionally left blank]\n")
  }

  func testMainWindow() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(
      mainWindow.display(),
      "Main Window\nPosition: (100, 100), Size: (400 x 300)\nThis is the main window\n")
  }

  func testPositionMove() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    var pos = Position()
    let newX = Int.random(in: 0...100)
    let newY = Int.random(in: 0...1000)
    pos.moveTo(newX: newX, newY: newY)
    XCTAssertTrue(pos.x == newX && pos.y == newY, "Expected: Position(x: \(newX), \(newY)), got Position(x: \(pos.x), \(pos.y))")
  }
  
  func testResize() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    var size = Size()
    let newWidth = Int.random(in: 0...100)
    let newHeight = Int.random(in: 0...1000)
    size.resize(newWidth: newWidth, newHeight: newHeight)
    XCTAssertTrue(size.width == newWidth && size.height == newHeight, "Expected: Size(x: \(newWidth), \(newHeight)), got Size(x: \(size.width), \(size.height))")
  }
  
  func testMoveValid() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let testWindow: Window = {
      let window = Window()
      window.title = "Test Window"
      window.contents = "test"
      window.resize(to: Size(width: 100, height: 100))
      window.move(to: Position(x: 100, y: 100))
      return window
    }()
    XCTAssertEqual(
      testWindow.display(),
      "Test Window\nPosition: (100, 100), Size: (100 x 100)\ntest\n")
  }

  func testMoveTooFar() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let testWindow: Window = {
      let window = Window()
      window.title = "Test Window"
      window.contents = "test"
      window.resize(to: Size(width: 100, height: 100))
      window.move(to: Position(x: 750, y: 650))
      return window
    }()
    XCTAssertEqual(
      testWindow.display(),
      "Test Window\nPosition: (700, 500), Size: (100 x 100)\ntest\n")
  }

  func testMoveNegative() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let testWindow: Window = {
      let window = Window()
      window.title = "Test Window"
      window.contents = "test"
      window.resize(to: Size(width: 100, height: 100))
      window.move(to: Position(x: -80, y: -60))
      return window
    }()
    XCTAssertEqual(
      testWindow.display(),
      "Test Window\nPosition: (0, 0), Size: (100 x 100)\ntest\n")
  }

  func testResizeValid() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let testWindow: Window = {
      let window = Window()
      window.title = "Test Window"
      window.contents = "test"
      window.move(to: Position(x: 600, y: 500))
      window.resize(to: Size(width: 100, height: 100))
      return window
    }()
    XCTAssertEqual(
      testWindow.display(),
      "Test Window\nPosition: (600, 500), Size: (100 x 100)\ntest\n")
  }

  func testResizeTooFar() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let testWindow: Window = {
      let window = Window()
      window.title = "Test Window"
      window.contents = "test"
      window.move(to: Position(x: 710, y: 525))
      window.resize(to: Size(width: 1000, height: 1000))
      return window
    }()
    XCTAssertEqual(
      testWindow.display(),
      "Test Window\nPosition: (710, 525), Size: (90 x 75)\ntest\n")
  }

  func testResizeNegative() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let testWindow: Window = {
      let window = Window()
      window.title = "Test Window"
      window.contents = "test"
      window.resize(to: Size(width: 0, height: -100))
      return window
    }()
    XCTAssertEqual(
      testWindow.display(),
      "Test Window\nPosition: (0, 0), Size: (1 x 1)\ntest\n")
  }

  func testUpdateTitle() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let window = Window()
    window.update(title: "Did it change?")
    XCTAssertEqual(
      window.display(),
      "Did it change?\nPosition: (0, 0), Size: (80 x 60)\n[This window intentionally left blank]\n")
  }

  func testUpdateText() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let window = Window()
    window.update(text: "Did it change?")
    XCTAssertEqual(
      window.display(), "New Window\nPosition: (0, 0), Size: (80 x 60)\nDid it change?\n")
  }

  func testUpdateTextNil() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let window = Window()
    window.update(text: "Did it change?")
    window.update(text: nil)
    XCTAssertEqual(
      window.display(),
      "New Window\nPosition: (0, 0), Size: (80 x 60)\n[This window intentionally left blank]\n")
  }

  static var allTests = [
    ("testNewWindow", testNewWindow),
    ("testMainWindow", testMainWindow),
    ("testMoveValid", testMoveValid),
    ("testMoveTooFar", testMoveTooFar),
    ("testMoveNegative", testMoveNegative),
    ("testResizeValid", testResizeValid),
    ("testResizeTooFar", testResizeTooFar),
    ("testResizeNegative", testResizeNegative),
    ("testUpdateTitle", testUpdateTitle),
    ("testUpdateText", testUpdateText),
    ("testUpdateTextNil", testUpdateTextNil),
  ]
}
