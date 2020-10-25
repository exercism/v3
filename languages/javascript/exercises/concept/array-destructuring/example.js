export function getFirstCard(deck) {
  const [first] = deck

  return first
}

export function getSecondCard(deck) {
  const [, second] = deck

  return second
}

export function swapTopTwoCards(deck) {
  ;[deck[0], deck[1]] = [deck[1], deck[0]]

  return deck
}

export function discardTopCard(deck) {
  const [first, ...rest] = deck

  return [first, rest]
}

export function insertFaceCards(deck) {
  const faceCards = ['jack', 'queen', 'king']
  const [topCard, ...rest] = deck

  return [topCard, ...faceCards, ...rest]
}
