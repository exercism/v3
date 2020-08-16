import XCTest

@testable import closures

final class closuresTests: XCTestCase {
  let runAll = Bool(ProcessInfo.processInfo.environment["RUNALL", default: "true"]) ?? false

  func testGenerateDigitSingle() {
    let (base, next) = generateDigit(7)
    XCTAssertTrue(
      base == 0 && next == 7,
      "generateDigit(7): Expected (base: 0, next: 7), got (base: \(base), next: \(next))")
  }

  func testGenerateDigitMulti() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let (base, next) = generateDigit(39)
    XCTAssertTrue(
      base == 3 && next == 9,
      "generateDigit(39): Expected (base: 3, next: 9), got (base: \(base), next: \(next))")
  }

  func testStopAtZero0() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertTrue(stopAtZero(0), "stopAtZero(0): Expected true, got false")
  }

  func testStopAtZero1() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertFalse(stopAtZero(1), "stopAtZero(1): Expected false, got true")
  }

  func testDigitsOf99231() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let digits = digitsOf(99231)
    let expected = [1, 3, 2, 9, 9]
    XCTAssertEqual(digits, expected, "digitsOf(99231): Expected \(expected), got \(digits)")
  }

  func testDigitsOf0() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let digits = digitsOf(0)
    let expected = [Int]()
    XCTAssertEqual(digits, expected, "digitsOf(0): Expected \(expected), got \(digits)")
  }

  func testGenerateBitSingle() {
    let (base, next) = generateDigit(7)
    XCTAssertTrue(
      base == 0 && next == 7,
      "generateDigit(7): Expected (base: 0, next: 7), got (base: \(base), next: \(next))")
  }

  func testGenerateBitMulti() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let (base, next) = generateDigit(39)
    XCTAssertTrue(
      base == 3 && next == 9,
      "generateDigit(39): Expected (base: 3, next: 9), got (base: \(base), next: \(next))")
  }

  func testBitsOf549() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let bits = bitsOf(549)
    let expected = [1, 0, 1, 0, 0, 1, 0, 0, 0, 1]
    XCTAssertEqual(bits, expected, "bitsOf(549): Expected \(expected), got \(bits)")
  }

  func testBitsOf0() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let bits = bitsOf(0)
    let expected = [Int]()
    XCTAssertEqual(bits, expected, "bitsOf(0): Expected \(expected), got \(bits)")
  }

  func testGeneratorForBaseValid() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let generators = (2...10).compactMap(generatorForBase)
    let digits = generators.flatMap { gen -> [Int] in
      let p = gen(237)
      return [p.0, p.1]
    }
    var (success, message) = (true, "")
    let expected = [118, 1, 79, 0, 59, 1, 47, 2, 39, 3, 33, 6, 29, 5, 26, 3, 23, 7]
    if let idx: Int =
      zip(digits, expected).enumerated().first(where: { $0.element.0 != $0.element.1 })?.offset
    {
      let base = idx / 2
      let next = base + 1
      success = false
      message =
        "generatorForBase(\(base + 2)): Expected: (base: \(expected[base]), next: \(expected[next])) got: (base: \(digits[base]), next: \(digits[next]))"
    }
    XCTAssert(success, message)
  }

  func testGeneratorForBaseInvalid() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let bases = [1, -4, 100, 11, 0]
    let generators = bases.map(generatorForBase)
    var (success, message) = (true, "")
    if let idx = generators.firstIndex(where: { $0 != nil }) {
      (success, message) = (false, "generatorForBase(\(bases[idx])): Expected: nil, got closure")
    }
    XCTAssert(success, message)
  }

  func testGenerateCollatzEven() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let (base, next) = generateCollatz(6)
    XCTAssertTrue(
      base == 3 && next == 6,
      "generateCollatz(6): Expected (base: 3, next: 6), got (base: \(base), next: \(next))")
  }

  func testGenerateCollatzOdd() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let (base, next) = generateCollatz(7)
    XCTAssertTrue(
      base == 22 && next == 7,
      "generateCollatz(6): Expected (base: 22, next: 7), got (base: \(base), next: \(next))")
  }

  func testStopAtOne0() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertFalse(stopAtOne(0), "stopAtOne(0): Expected false, got true")
  }

  func testStopAtOne1() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    XCTAssertTrue(stopAtOne(1), "stopAtOne(1): Expected true, got false")
  }

  func testCollatz11() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let seq = collatz(11)
    let expected = [11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2]
    XCTAssertEqual(seq, expected, "collatz(11): Expected \(expected), got \(seq)")
  }

  func testCollatzZero() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let seq = collatz(0)
    let expected = [Int]()
    XCTAssertEqual(seq, expected, "collatz(0): Expected \(expected), got \(seq)")
  }

  static var allTests = [
    ("testGenerateDigitSingle", testGenerateDigitSingle),
    ("testGenerateDigitMulti", testGenerateDigitMulti),
    ("testStopAtZero0", testStopAtZero0),
    ("testStopAtZero1", testStopAtZero1),
    ("testDigitsOf99231", testDigitsOf99231),
    ("testDigitsOf0", testDigitsOf0),
    ("testBitsOf549", testBitsOf549),
    ("testBitsOf0", testBitsOf0),
    ("testGeneratorForBaseValid", testGeneratorForBaseValid),
    ("testGeneratorForBaseInvalid", testGeneratorForBaseInvalid),
    ("testGenerateCollatzEven", testGenerateCollatzEven),
    ("testGenerateCollatzOdd", testGenerateCollatzOdd),
    ("testStopAtOne0", testStopAtOne0),
    ("testStopAtOne1", testStopAtOne1),
    ("testCollatz11", testCollatz11),
    ("testCollatzZero", testCollatzZero),
  ]
}

//let collatzGenerator = { (n: Int) -> (base: Int, value: Int) in
//  (base: n.isMultiple(of: 2) ? n / 2 : 3 * n + 1, value: n)
//}
//
//let stopAtOne = { (n: Int) -> Bool in n == 1 }
//
//let collatz = { (n: Int) -> [Int] in
//  unroll(n, generator: collatzGenerator, stop: stopAtOne)
//}
