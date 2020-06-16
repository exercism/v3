using System;
static class SimpleCalculator
{
    public static string ErrorLog {get; set;}
    public static int Calculate(int operand1, int operand2, string operation)
    {
        int result = 0;
        ErrorLog = "";
        try
        {
            switch(operation)
            {
                case "+":
                    result = Addition(operand1, operand2);
                    break;
                case "*":
                    result = Multiplication(operand1, operand2);
                    break;
                case "/":
                    result = Division(operand1, operand2);
                    break;
                default:
                    throw new InvalidOperationException($"Operation {operation} does not exist");
            }
        }
        catch(ArgumentOutOfRangeException e)
        {
            ErrorLog = $"Result invalid: {e.Message}";
            return -1;
        }
        return result;

    }

    private static int Division(int operand1, int operand2)
    {
        return operand1 / operand2;
    }

    private static int Multiplication(int operand1, int operand2)
    {
        int result = operand1 * operand2;
        if(operand1 > 0 && operand2 > 0 && result < 0)
        {
            throw new ArgumentException("Result of operation does not fit in type of int.");
        }
        return result;
    }
    private static int Addition(int operand1, int operand2)
    {
        int result = operand1 + operand2;
        if(operand1 > 0 && operand2 > 0 && result < 0)
        {
            throw new ArgumentException("Result of operation does not fit in type of int.");
        }
        return result;
    }

}