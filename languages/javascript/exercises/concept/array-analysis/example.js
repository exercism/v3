export function getCardIndex(array, card) {
  return array.indexOf(card)
}

export function isStackIncludes(array, cardGiven) {
  return array.includes(cardGiven)
}

export function isEvenStack(array) {
  return array.every((card) => card % 2 === 0)
}

export function isStackContainsOdd(array) {
  return array.some((card) => card % 2 !== 0)
}

export function getFirstOddCard(array) {
  return array.find((card) => card % 2 !== 0)
}

export function getFirstEvenCardIndex(array) {
  return array.findIndex((card) => card % 2 === 0)
}
