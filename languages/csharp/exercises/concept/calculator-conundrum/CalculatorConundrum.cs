using System;

public static class CalculatorConundrum
{
    public static EuclideanDivisionResult DivideRemainder(EuclideanDivision euclideanDivision)
    {
        throw new NotImplementedException("Please implement the CalculatorConundrum.DivideRemainder() method");
    }
}

/**** Please do not modify the code below ****/
public class EuclideanDivision
{
    public int Dividend { get; set; }
    public int Divisor { get; set; }
    public EuclideanDivision(int dividend, int divisor)
    {
        Dividend = dividend;
        Divisor = divisor;
    }
}

/**** Please do not modify the code below ****/
public class EuclideanDivisionResult
{
    public int Quotient { get; set; }
    public int Remainder { get; set; }
    public EuclideanDivisionResult(int quotient, int remainder)
    {
        Quotient = quotient;
        Remainder = remainder;
    }
}
