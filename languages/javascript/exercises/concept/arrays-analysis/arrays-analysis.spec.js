import {
  getCardIndex,
  isStackIncludes,
  isEvenStack,
  isStackContainsOdd,
  getFirstOddCard,
  getFirstEvenCardIndex
} from './example'

describe('arrays-analysis', () => {
  describe('getCardIndex', () => {
    const getCardIndexTestCases = [
      [[1, 2, 3], 1, 0],
      [[1, 2, 2], 2, 1],
      [[1, 2, 3], 4, -1]
    ]

    getCardIndexTestCases.forEach(([array, item, expected]) => {
      test(`getCardIndex([${array}], ${item})`, () => {
        expect(getCardIndex(array, item)).toStrictEqual(expected)
      })
    })
  })

  describe('isStackIncludes', () => {
    const isStackIncludesTestCases = [
      [[1, 2, 3], 1, true],
      [[1, 2, 3], 4, false]
    ]

    isStackIncludesTestCases.forEach(([array, item, expected]) => {
      test(`isStackIncludes([${array}],${item})`, () => {
        expect(isStackIncludes(array, item)).toBe(expected)
      })
    })
  })

  describe('isEvenStack', () => {
    const isEvenStackTestCases = [
      [[1], false],
      [[2, 5], false],
      [[2, 4, 8, 6], true]
    ]

    isEvenStackTestCases.forEach(([array, expected]) => {
      test(`isEvenStack([${array}])`, () => {
        expect(isEvenStack(array)).toStrictEqual(expected)
      })
    })
  })

  describe('isStackContainsOdd', () => {
    const isStackContainsOddTestCases = [
      [[2, 4, 6], false],
      [[2, 5], true],
      [[1, 3, 5, 7], true]
    ]

    isStackContainsOddTestCases.forEach(([array, expected]) => {
      test(`isStackContainsOdd([${array}])`, () => {
        expect(isStackContainsOdd(array)).toStrictEqual(expected)
      })
    })
  })

  describe('getFirstOddCard', () => {
    const getFirstOddCardTestCases = [
      [[2, 4, 1, 3], 1],
      [[1, 2], 1],
      [[4, 2, 6], undefined]
    ]

    getFirstOddCardTestCases.forEach(([array, expected]) => {
      test(`getFirstOddCard([${array}])`, () => {
        expect(getFirstOddCard(array)).toStrictEqual(expected)
      })
    })
  })

  describe('getFirstEvenCardIndex', () => {
    const getFirstEvenCardIndexTestCases = [
      [[2, 4, 1, 3], 0],
      [[1, 2], 1],
      [[1, 3, 5], -1]
    ]

    getFirstEvenCardIndexTestCases.forEach(([array, expected]) => {
      test(`getFirstEvenCardIndex([${array}])`, () => {
        expect(getFirstEvenCardIndex(array)).toStrictEqual(expected)
      })
    })
  })
})
