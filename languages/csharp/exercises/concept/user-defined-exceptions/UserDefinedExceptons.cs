using System;

public class CalculationException_template : Exception
{
    private int operand;

    public CalculationException_template(int operand, string message, Exception inner)
    // TODO: complete the definition of the constructor
    {

    }

    public int GetOperand()
    {
        throw new NotImplementedException($"Please implement the GetOperand() method");
    }
}

public class CalculatorTestHarness_template
{
    public string Run(string testName, int testValue)
    {
        throw new NotImplementedException($"Please implement the Run() method");
    }

    public void Calculate(int testNum)
    {
    }
}

// Please do not modify the code below.  In a more realistic
// scenario this would be in a different library and would actually
// do something meaningful.  This code will always throw
// a Sysem.OverflowException
public static class TestHarnessOperations_template
{
    public static int HandleInt(int testNum)
    {
        int trouble = Int32.MaxValue;
        checked
        {
            return Int32.MaxValue * trouble;
        }
    }
}
