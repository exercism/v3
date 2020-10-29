import GetOrganizedTests
import XCTest

var tests = [XCTestCaseEntry]()
tests += GetOrganizedTests.allTests()
XCTMain(tests)
