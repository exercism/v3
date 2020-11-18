import XCTest

#if !canImport(ObjectiveC)
  public func allTests() -> [XCTestCaseEntry] {
    return [
      testCase(FreelancerRatesTests.allTests)
    ]
  }
#endif
