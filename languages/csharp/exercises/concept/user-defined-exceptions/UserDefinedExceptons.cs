using System;

public class CalculationException : Exception
{
    public CalculationException(int operand1, int operand2, string message, Exception inner) : base(message, inner)
    // TODO: complete the definition of the constructor
    {
    }

    public int Operand1 { get; }
    public int Operand2 { get; }
}

public class CalculatorTestHarness
{
    private Calculator calculator;

    public CalculatorTestHarness_template(Calculator_template calculator)
    {
        this.calculator = calculator;
    }

    public string Multiply(int x, int y)
    {
        throw new NotImplementedException($"Please implement the Multiply() method");
    }

    public void TestMultiplication(int x, int y)
    {
        throw new NotImplementedException($"Please implement the TestMultiplication() method");
    }
}

// Please do not modify the code below.
// If there is an overflow in the multiplication operation
// then a System.OverflowException is thrown.
public class Calculator_template
{
    public int Multiply(int x, int y)
    {
        checked
        {
            return x * y;
        }
    }
}
