import XCTest

@testable import LogLines

final class LogLinesTests: XCTestCase {
  let runAll = Bool(ProcessInfo.processInfo.environment["RUNALL", default: "false"]) ?? false

  func testInitTrace() {
    let line = "[TRC]: Line 84 - Console.WriteLine('Hello World');"
    XCTAssertEqual(LogLevel(line), LogLevel.trace)
  }

  func testInitDebug() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let line = "[DBG]: ; expected"
    XCTAssertEqual(LogLevel(line), LogLevel.debug)
  }

  func testInitInfo() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let line = "[INF]: Timezone changed"
    XCTAssertEqual(LogLevel(line), LogLevel.info)
  }

  func testInitWarning() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let line = "[WRN]: Timezone not set"
    XCTAssertEqual(LogLevel(line), LogLevel.warning)
  }

  func testInitError() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let line = "[ERR]: Disk full"
    XCTAssertEqual(LogLevel(line), LogLevel.error)
  }

  func testInitFatal() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let line = "[FTL]: Not enough memory"
    XCTAssertEqual(LogLevel(line), LogLevel.fatal)
  }

  func testInitUnknownEmpty() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let line = "Something terrible has happened!"
    XCTAssertEqual(LogLevel(line), LogLevel.unknown)
  }

  func testInitUnknownNonStandard() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let line = "[XYZ]: Gibberish message.. beep boop.."
    XCTAssertEqual(LogLevel(line), LogLevel.unknown)
  }

  func testShortTrace() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let message = "Line 13 - int myNum = 42;"
    XCTAssertEqual(LogLevel.trace.shortFormat(message: message), "0:Line 13 - int myNum = 42;")
  }

  func testShortDebug() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let message = "The name 'LogLevel' does not exist in the current context"
    XCTAssertEqual(
      LogLevel.debug.shortFormat(message: message),
      "1:The name 'LogLevel' does not exist in the current context")
  }

  func testShortInfo() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let message = "File moved"
    XCTAssertEqual(LogLevel.info.shortFormat(message: message), "4:File moved")
  }

  func testShortWarning() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let message = "Unsafe password"
    XCTAssertEqual(LogLevel.warning.shortFormat(message: message), "5:Unsafe password")
  }

  func testShortError() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let message = "Stack overflow"
    XCTAssertEqual(LogLevel.error.shortFormat(message: message), "6:Stack overflow")
  }

  func testShortFatal() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let message = "Dumping all files"
    XCTAssertEqual(LogLevel.fatal.shortFormat(message: message), "7:Dumping all files")
  }

  func testShortUnknownEmpty() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let message = "Wha happon?"
    XCTAssertEqual(LogLevel.unknown.shortFormat(message: message), "42:Wha happon?")
  }

  static var allTests = [
    ("testInitTrace", testInitTrace),
    ("testInitDebug", testInitDebug),
    ("testInitInfo", testInitInfo),
    ("testInitWarning", testInitWarning),
    ("testInitError", testInitError),
    ("testInitFatal", testInitFatal),
    ("testInitUnknownEmpty", testInitUnknownEmpty),
    ("testInitUnknownNonStandard", testInitUnknownNonStandard),
    ("testShortTrace", testShortTrace),
    ("testShortDebug", testShortDebug),
    ("testShortInfo", testShortInfo),
    ("testShortWarning", testShortWarning),
    ("testShortError", testShortError),
    ("testShortFatal", testShortFatal),
    ("testShortUnknownEmpty", testShortUnknownEmpty),
  ]
}
