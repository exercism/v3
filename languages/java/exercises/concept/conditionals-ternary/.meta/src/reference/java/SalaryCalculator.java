public class SalaryCalculator {

    private final double BASESALARY = 1000.0;
    private final double MAXIMUMSALARY = 2000.0;

    public double multiplierPerDaysSkipped (int daysSkipped) {
        return daysSkipped < 5 ? 1 : 0.85;
    }

    public int multiplierPerProductsSold (int productsSold) {
        return productsSold < 20 ? 10 : 13;
    }

    public double bonusProductSold (int productsSold) {
        return productsSold * multiplierPerProductsSold(productsSold);
    }

    public double finalSalary (int daysSkipped, int productsSold) {

        double finalSalary = BASESALARY * multiplierPerDaysSkipped(daysSkipped) + bonusProductSold(productsSold);

        return finalSalary > MAXIMUMSALARY ? MAXIMUMSALARY : finalSalary;
    } 

}
