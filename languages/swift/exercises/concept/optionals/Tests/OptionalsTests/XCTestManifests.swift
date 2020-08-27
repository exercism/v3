import XCTest

#if !canImport(ObjectiveC)
  public func allTests() -> [XCTestCaseEntry] {
    return [
      testCase(OptionalsTests.allTests)
    ]
  }
#endif
