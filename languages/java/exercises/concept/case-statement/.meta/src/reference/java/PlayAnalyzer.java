public class PlayAnalyzer{

    //enhanced switch implementation
    public static String analyseOnField(int shirtNum){
        String playerDescription = switch (shirtNum) {
            case 1 -> "goalie";
            case 2 -> "left back";
            case 5 -> "right back";
            case 3, 4 -> "center back";
            case 6, 7, 8 -> "midfielder";
            case 9 -> "left wing";
            case 11 -> "right wing";
            case 10 -> "striker";
            default -> throw new IllegalArgumentException();
        };
        return playerDescription;
    }
    
    //Traditionnal switch implementation
/*     public static String analyseOnField(int shirtNum){
        String playerDescription = "";
        switch (shirtNum) {
            case 1:
                playerDescription = "goalie";
                break;
            case 2:
                playerDescription = "left back";
                break;
            case 5:
                playerDescription = "right back";
                break;
            case 3:
            case 4:
                playerDescription = "center back";
                break;
            case 6:
            case 7:
            case 8:
                playerDescription = "midfielder";
                break;
            case 9:
                playerDescription = "left wing";
                break;
            case 11:
                playerDescription = "right wing";
                break;
            case 10:
                playerDescription = "striker";
                break;
            default:
                throw new IllegalArgumentException();
        }
        return playerDescription;
    } */
}
