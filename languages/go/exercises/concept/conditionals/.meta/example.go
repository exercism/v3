package _meta

// ParseCard returns the integer value of a card following blackjack ruleset.
func ParseCard(card string) int {
	switch card {
	case "ace":
		return 11
	case "two":
		return 2
	case "three":
		return 3
	case "four":
		return 4
	case "five":
		return 5
	case "six":
		return 6
	case "seven":
		return 7
	case "eight":
		return 8
	case "nine":
		return 9
	case "ten", "jack", "queen", "king":
		return 10
	default:
		return 0
	}
}

// Blackjack returns true if the player has a blackjack, false otherwise.
func Blackjack(card1, card2 string) bool {
	if ParseCard(card1)+ParseCard(card2) == 21 {
		return true
	}
	return false
}

// Pair returns true if the player has a pair of cards, false otherwise.
func Pair(card1, card2 string) bool {
	if card1 == card2 {
		return true
	}
	return false
}

// FirstTurn returns the "optimal" decision for the first turn, given the cards of the player and the dealer.
func FirstTurn(card1, card2, dealerCard string) string {
	handScore := ParseCard(card1) + ParseCard(card2)
	if Pair(card1, card2) && handScore != 20 {
		return "P"
	}
	dealerScore := ParseCard(dealerCard)
	if Blackjack(card1, card2) {
		if dealerScore < 10 {
			return "W"
		}
		return "S"
	}

	switch {
	case handScore >= 17:
		return "S"
	case handScore >= 13 && dealerScore <= 6:
		return "S"
	case handScore >= 13 && dealerScore > 6:
		return "H"
	case handScore == 12 && (dealerScore <= 3 || dealerScore > 6):
		return "H"
	case handScore == 12 && dealerScore > 3 && dealerScore <= 6:
		return "S"
	case handScore >= 10:
		return "D"
	case handScore == 9 && (dealerScore <= 2 || dealerScore > 6):
		return "H"
	case handScore == 9 && dealerScore > 2 && dealerScore <= 6:
		return "D"
	default:
		return "H"
	}
}
