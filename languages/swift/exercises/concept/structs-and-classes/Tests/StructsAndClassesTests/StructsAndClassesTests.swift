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
      "Main Window\nPosition: (400, 400), Size: (400 x 200)\nThis is the main window\n")
  }

  func testHelpWindow() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertEqual(
      helpWindow.display(),
      "Help Window\nPosition: (590, 10), Size: (200 x 200)\nSomebody called for help?\n")
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
  
  func testMoveToValid() throws {
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

  func testMoveToTooFar() throws {
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

  func testMoveToNegative() throws {
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

  func testMoveDeltaValid() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let testWindow: Window = {
      let window = Window()
      window.title = "Test Window"
      window.contents = "test"
      window.resize(to: Size(width: 100, height: 100))
      window.move(deltaX: 400, deltaY: 400)
      return window
    }()
    XCTAssertEqual(
      testWindow.display(),
      "Test Window\nPosition: (400, 400), Size: (100 x 100)\ntest\n")
  }

  func testMoveDeltaTooFar() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let testWindow: Window = {
      let window = Window()
      window.title = "Test Window"
      window.contents = "test"
      window.resize(to: Size(width: 500, height: 500))
      window.move(deltaX: 400, deltaY: 400)
      return window
    }()
    XCTAssertEqual(
      testWindow.display(),
      "Test Window\nPosition: (300, 100), Size: (500 x 500)\ntest\n")
  }

  func testMoveDeltaNegative() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let testWindow: Window = {
      let window = Window()
      window.title = "Test Window"
      window.contents = "test"
      window.resize(to: Size(width: 100, height: 100))
      window.move(deltaX: -1000, deltaY: -2)
      return window
    }()
    XCTAssertEqual(
      testWindow.display(),
      "Test Window\nPosition: (0, 0), Size: (100 x 100)\ntest\n")
  }

  func testResizeToValid() throws {
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

  func testResizeToTooFar() throws {
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

  func testResizeToNegative() throws {
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

  func testResizeDeltaValid() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let testWindow: Window = {
      let window = Window()
      window.title = "Test Window"
      window.contents = "test"
      window.move(deltaX: 400, deltaY: 400)
      window.resize(deltaW: 20, deltaH: 40)
      return window
    }()
    XCTAssertEqual(
      testWindow.display(),
      "Test Window\nPosition: (400, 400), Size: (100 x 100)\ntest\n")
  }

  func testResizeDeltaTooFar() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let testWindow: Window = {
      let window = Window()
      window.title = "Test Window"
      window.contents = "test"
      window.move(deltaX: 400, deltaY: 400)
      window.resize(deltaW: 400, deltaH: 300)
      return window
    }()
    XCTAssertEqual(
      testWindow.display(),
      "Test Window\nPosition: (400, 400), Size: (400 x 200)\ntest\n")
  }

  func testResizeDeltaNegative() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let testWindow: Window = {
      let window = Window()
      window.title = "Test Window"
      window.contents = "test"
      window.resize(to: Size(width: -100, height: -80))
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
    ("testHelpWindow", testHelpWindow),
    ("testMoveToValid", testMoveToValid),
    ("testMoveToTooFar", testMoveToTooFar),
    ("testMoveToNegative", testMoveToNegative),
    ("testMoveDeltaValid", testMoveDeltaValid),
    ("testMoveDeltaTooFar", testMoveDeltaTooFar),
    ("testMoveDeltaNegative", testMoveDeltaNegative),
    ("testResizeToValid", testResizeToValid),
    ("testResizeToTooFar", testResizeToTooFar),
    ("testResizeToNegative", testResizeToNegative),
    ("testResizeDeltaValid", testResizeDeltaValid),
    ("testResizeDeltaTooFar", testResizeDeltaTooFar),
    ("testResizeDeltaNegative", testResizeDeltaNegative),
    ("testUpdateTitle", testUpdateTitle),
    ("testUpdateText", testUpdateText),
    ("testUpdateTextNil", testUpdateTextNil),
  ]
}
