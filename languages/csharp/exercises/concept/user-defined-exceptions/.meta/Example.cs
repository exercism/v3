using System;

public class CalculationException : Exception
    // TODO: complete the definition of the constructor
{
    private int operand;
    public CalculationException(int operand, string message, Exception inner) : base(message, inner)
    {
        this.operand = operand;
    }

    public int GetOperand()
    {
        return operand;
    }
}

public class CalculatorTestHarness
{
    public string Run(string testName, int testValue)
    {
        try
        {
            if (testName == "Calculate")
            {
                Calculate(testValue);
            }
            else if (testName == "HandleInt")
            {
                TestHarnessOperations.HandleInt(testValue);
            }
        }
        catch (OverflowException ofex)
        {
            return "HandleInt failure";
        }
        catch (CalculationException cex) when (cex.GetOperand() == 0)
        {
            return "Calculate failed for a zero value. " + cex.InnerException.Message;
        }
        catch (CalculationException cex)
        {
            return "Calculate failed for a non-zero value. " + cex.InnerException.Message;
        }

        return string.Empty;
    }

    public void Calculate(int testNum)
    {
        try
        {
            TestHarnessOperations.HandleInt(testNum);
        }
        catch (OverflowException ofex)
        {
            throw new CalculationException(testNum, "Calculate: operation failed in FakeOp", ofex);
        }
    }
}


// Please do not modify the code below.  In a more realistic
// scenario this would be in a different library and would actually
// do something meaningful.  This code will always throw
// a Sysem.OverflowException
public static class TestHarnessOperations
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
