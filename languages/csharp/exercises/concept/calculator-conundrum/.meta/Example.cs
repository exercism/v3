using System;

public static class CalculatorConundrum
{
    public static EuclideanDivisionResult DivideRemainder(EuclideanDivision euclideanDivision)
    {
        if (euclideanDivision == null) throw new ArgumentNullException(nameof(euclideanDivision), "Object cannot be null.");
        if (euclideanDivision.Divisor == 0) throw new DivideByZeroException("Division by zero is not allowed.");
        var quotient = euclideanDivision.Dividend / euclideanDivision.Divisor;
        var remainder = euclideanDivision.Dividend % euclideanDivision.Divisor;
        return new EuclideanDivisionResult(quotient, remainder);
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
