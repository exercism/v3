using Xunit;

// TODO: convert Theory-based tests to Fact-based tests.
// This is necessary in order to be able to display the 
// input for which the test failed, which is defined in
// the .meta/config.json file
public class SavingsAccountTest
{
    public static TheoryData<decimal, float> AnnualPercentageYieldData = new TheoryData<decimal, float>
    {
        { -152964.231m, -3.213f },
        { -300.0m, -3.213f },
        { -0.123m, -3.213f },
        { 0m, 0.5f },
        { 0.000001m, 0.5f },
        { 0.012345m, 0.5f },
        { 200.75m, 0.5f },
        { 523.75m, 0.5f },
        { 999.9999m, 0.5f },
        { 1_000.0m, 1.621f },
        { 1_000.0001m, 1.621f },
        { 4_999.9990m, 1.621f },
        { 5_000.0000m, 2.475f },
        { 5_000.0001m, 2.475f },
        { 9_999.999999m, 2.475f },
        { 10_000.0m, 2.475f },
        { 10_000.000001m, 2.475f },
        { 5_639_998.742909m, 2.475f }
    };

    [Theory]
    [MemberData(nameof(AnnualPercentageYieldData))]
    public void AnnualPercentageYield(decimal balance, float expected) =>
        Assert.Equal(expected, SavingsAccount.AnnualPercentageYield(balance));

    public static TheoryData<decimal, decimal> AnnualBalanceUpdateData = new TheoryData<decimal, decimal>
    {
        { -152964.231m, -157878.97174203m },
        { -300.0m, -309.639000m },
        { -0.123m, -0.12695199m },
        { 0.0m, 0.0000m },
        { 0.000001m, 0.000001005m },
        { 0.012345m, 0.012406725m },
        { 200.75m, 201.75375m },
        { 999.9999m, 1004.9998995m },
        { 1_000.0m, 1016.210000m },
        { 1_000.0001m, 1016.210101621m },
        { 5_000.0m, 5123.750000m },
        { 5_000.0001m, 5123.750102475m },
        { 9_999.999999m, 10247.49999897525m },
        { 10_000.0m, 10247.500000m },
        { 10_000.000001m, 10247.50000102475m },
        { 5_639_998.742909m, 5779588.71179599775m },
        { 876_432_317.9568123m, 898124017.826243404425m }
    };

    [Theory]
    [MemberData(nameof(AnnualBalanceUpdateData))]
    public void AnnualBalanceUpdate(decimal balance, decimal expected) =>
        Assert.Equal(expected, SavingsAccount.AnnualBalanceUpdate(balance));

    public static TheoryData<decimal, decimal, int> YearsBeforeDesiredBalanceData = new TheoryData<decimal, decimal, int>
    {
        { 100.0m, 125.80m, 47 },
        { 200.75m, 214.88m, 14 },
        { 1_000.0m, 1_100.0m, 6 },
        { 1_000.0m, 1_200.0m, 12 },
        { 1_000.0m, 1_500.0m, 26 },
        { 2_345.67m, 12_345.6789m, 85 },
        { 8_080.80m, 9_090.90m, 5 },
        { 9_526.75m, 10_176.25m, 3 },
        { 25_000.00m, 27_000.0m, 4 }
    };

    [Theory]
    [MemberData(nameof(YearsBeforeDesiredBalanceData))]
    public void YearsBeforeDesiredBalance(decimal currentBalance, decimal desiredBalance, int expected) =>
        Assert.Equal(expected, SavingsAccount.YearsBeforeDesiredBalance(currentBalance, desiredBalance));
}