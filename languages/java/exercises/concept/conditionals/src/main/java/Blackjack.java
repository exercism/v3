public class Blackjack{

    public int  ParseCard(String card) {
        throw new UnsupportedOperationException("Please implement the Blackjack.ParseCard method");
    }

    public boolean IsBlackjack(String card1, String card2) {
        throw new UnsupportedOperationException("Please implement the Blackjack.IsBlackjack method");
    }

    public String LargeHand(boolean isBlackjack, int dealerScore) {
        throw new UnsupportedOperationException("Please implement the Blackjack.LargeHand method");
    }

    public String SmallHand(handScore int, dealerScore int) {
        throw new UnsupportedOperationException("Please implement the Blackjack.SmallHand method");
    }

    // FirstTurn returns the semi-optimal decision for the first turn, given the cards of the player and the dealer.
    // This function is already implemented and does not need to be edited. It pulls the other functions together in a
    // complete decision tree for the first turn.
    public String  FirstTurn(String card1, String card2, String dealerCard) {
        int handScore = ParseCard(card1) + ParseCard(card2);
        int dealerScore = ParseCard(dealerCard);

        if (20 < handScore) 
            return LargeHand(IsBlackjack(card1, card2), dealerScore);
        
        else
            return SmallHand(handScore, dealerScore);
    }
}