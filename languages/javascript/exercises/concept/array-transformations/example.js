/**
 * Double every card in the deck
 *
 * @param {number[]} deck
 *
 * @returns {number[]} deck with every card doubled
 */
export function doubleCards(deck) {
  return deck.map((card) => card * 2)
}

/**
 *  Creates triplicates of every 3 found in the deck
 *
 * @param {number[]} deck
 *
 * @returns {number[]} deck with triplicate 3s
 */
export function threeOfThree(deck) {
  return deck.reduce((newDeck, card) => {
    if (card === 3) {
      newDeck.push(3, 3, 3)
    } else {
      newDeck.push(card)
    }
  }, [])
}

/**
 * Removes every card from the deck but the middle two
 * Assumes a deck is always 10 cards
 *
 * @param {number[]} deck of 10 cards
 *
 * @returns {number[]} deck with only two middle cards
 */
export function middleTwo(deck) {
  return deck.slice(4, 5)
}

/**
 * Moves the outside two cards to the middle
 *
 * @param {number[]} deck with 10 cards
 *
 * @returns {number[]} transformed deck
 */

export function sandwichTrick(deck) {
  const firstCard = deck[0]
  const lastCard = deck[9]
  const withInsertedCards = deck.splice(4, 0, lastCard, firstCard)
  return withInsertedCards.slice(1, 10)
}

/**
 * Removes every card except the 2
 *
 * @param {number[]} deck
 *
 * @returns {number[]} deck with only the card 2
 */
export function twoIsSpecial(deck) {
  return deck.filter((card) => card === 2)
}

/**
 * Returns a perfectly order deck from lowest to highest
 *
 * @param {number[]} shuffled deck
 *
 * @returns {number[]} ordered deck
 */
export function perfectlyOrdered(deck) {
  return deck.sort()
}
