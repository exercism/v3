using System;

public class CalculationException : Exception
{
    public CalculationException(int operand1, int operand2, string message, Exception inner) : base(message, inner)
    // TODO: complete the definition of the constructor
    {
        Operand1 = operand1;
        Operand2 = operand2;
    }

    public int Operand1 { get; }
    public int Operand2 { get; }
}

public class CalculatorTestHarness
{
    private Calculator calculator;

    public CalculatorTestHarness(Calculator calculator)
    {
        this.calculator = calculator;
    }

    public string Multiply(int x, int y)
    {
        try
        {
            TestMultiplication(x, y);
        }
        catch (CalculationException cex) when (cex.Operand1 < 0)
        {
            return "Multiply failed for a negative value. " + cex.InnerException.Message;
        }
        catch (CalculationException cex)
        {
            return "Multiply failed for a positive value. " + cex.InnerException.Message;
        }

        return "Multiply succeeded";
    }

    public void TestMultiplication(int x, int y)
    {
        try
        {
            calculator.Multiply(x, y);
        }
        catch (OverflowException ofex)
        {
            throw new CalculationException(x, y, string.Empty, ofex);
        }
    }
}

// Please do not modify the code below.
// If there is an overflow in the multiplication operation
// then a System.OverflowException is thrown.
public class Calculator
{
    public int Multiply(int x, int y)
    {
        checked
        {
            return x * y;
        }
    }
}
