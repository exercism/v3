import XCTest

@testable import MagicianInTraining

final class MagicianInTrainingTests: XCTestCase {
  let runAll = Bool(ProcessInfo.processInfo.environment["RUNALL", default: "false"]) ?? false

  func testGetCard() {
    let stack = (1...10).shuffled()
    guard let idx = stack.indices.randomElement() else { fatalError("test suite failure") }
    XCTAssertEqual(getCard(at: idx, from: stack), stack[idx])
  }

  func testSetCard() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let stack = [9, 4, 3, 6, 1, 7, 2, 8, 5]
    let idx = 5
    XCTAssertEqual(setCard(at: idx, in: stack, to: 10), [9, 4, 3, 6, 1, 10, 2, 8, 5])
  }

  func testSetCardIndexTooLow() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let stack = [9, 4, 3, 6, 1, 7, 2, 8, 5]
    let idx = -3
    XCTAssertEqual(setCard(at: idx, in: stack, to: 10), stack)
  }

  func testSetCardIndexTooHigh() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let stack = [9, 4, 3, 6, 1, 7, 2, 8, 5]
    let idx = 50
    XCTAssertEqual(setCard(at: idx, in: stack, to: 10), stack)
  }

  func testInsertAtTop() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let stack = [1, 7, 5, 8, 3, 9, 6, 4, 2]
    XCTAssertEqual(insert(10, atTopOf: stack), [1, 7, 5, 8, 3, 9, 6, 4, 2, 10])
  }

  func testRemoveCard() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let stack = [9, 2, 1, 6, 5, 7, 4, 3, 8]
    let idx = 2
    XCTAssertEqual(removeCard(at: idx, from: stack), [9, 2, 6, 5, 7, 4, 3, 8])
  }

  func testRemoveCardIndexTooLow() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let stack = [9, 2, 1, 6, 5, 7, 4, 3, 8]
    let idx = -2
    XCTAssertEqual(removeCard(at: idx, from: stack), stack)
  }

  func testRemoveCardIndexTooHigh() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let stack = [9, 2, 1, 6, 5, 7, 4, 3, 8]
    let idx = 20
    XCTAssertEqual(removeCard(at: idx, from: stack), stack)
  }

  func testRemoveTopCard() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let stack = [2, 7, 4, 6, 9, 1, 8, 3, 5]
    XCTAssertEqual(removeTopCard(stack), [2, 7, 4, 6, 9, 1, 8, 3])
  }

  func testRemoveTopCardFromEmptyStack() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let stack = [Int]()
    XCTAssertEqual(removeTopCard(stack), stack)
  }

  func testInsertAtBottom() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let stack = [4, 3, 8, 9, 1, 7, 6, 5, 2]
    XCTAssertEqual(insert(10, atBottomOf: stack), [10, 4, 3, 8, 9, 1, 7, 6, 5, 2])
  }

  func testRemoveBottomCard() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let stack = [8, 7, 4, 2, 6, 5, 3, 1, 9]
    XCTAssertEqual(removeBottomCard(stack), [7, 4, 2, 6, 5, 3, 1, 9])
  }

  func testRemoveBottomCardFromEmptyStack() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let stack = [Int]()
    XCTAssertEqual(removeTopCard(stack), stack)
  }

  func testCheckSizeTrue() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let stack = [6, 9, 7, 8, 2, 3, 4, 5, 1]
    XCTAssertTrue(checkSizeOfStack(stack, 9))
  }

  func testCheckSizeFalse() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let stack = [6, 9, 7, 8, 2, 3, 4, 5, 1]
    XCTAssertFalse(checkSizeOfStack(removeBottomCard(stack), 9))
  }

  func testEvenCardCount() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let stack = [4, 6, 3, 7, 1, 9, 5, 8, 2]
    XCTAssertEqual(evenCardCount(stack), 4)
  }

  func testEvenCardCountZero() throws {
    try XCTSkipIf(true && !runAll)  // change true to false to run this test
    let stack = [7, 3, 7, 1, 5, 5, 3, 9, 9]
    XCTAssertEqual(evenCardCount(stack), 0)
  }

  static var allTests = [
    ("testGetCard", testGetCard),
    ("testSetCard", testSetCard),
    ("testSetCardIndexTooLow", testSetCardIndexTooLow),
    ("testSetCardIndexTooHigh", testSetCardIndexTooHigh),
    ("testInsertAtTop", testInsertAtTop),
    ("testRemoveCard", testRemoveCard),
    ("testRemoveCardIndexTooLow", testRemoveCardIndexTooLow),
    ("testRemoveCardIndexTooHigh", testRemoveCardIndexTooHigh),
    ("testRemoveTopCard", testRemoveTopCard),
    ("testRemoveTopCardFromEmptyStack", testRemoveTopCardFromEmptyStack),
    ("testInsertAtBottom", testInsertAtBottom),
    ("testRemoveBottomCard", testRemoveBottomCard),
    ("testRemoveBottomCardFromEmptyStack", testRemoveBottomCardFromEmptyStack),
    ("testCheckSizeTrue", testCheckSizeTrue),
    ("testCheckSizeFalse", testCheckSizeFalse),
    ("testEvenCardCount", testEvenCardCount),
    ("testEvenCardCountZero", testEvenCardCountZero),
  ]
}
