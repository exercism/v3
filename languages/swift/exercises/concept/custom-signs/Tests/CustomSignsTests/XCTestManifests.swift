import XCTest

#if !canImport(ObjectiveC)
  public func allTests() -> [XCTestCaseEntry] {
    return [
      testCase(CustomSignsTests.allTests)
    ]
  }
#endif
