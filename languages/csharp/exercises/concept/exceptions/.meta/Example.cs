using System;
static class SimpleCalculator
{
    public static string Calculate(int operand1, int operand2, string operation)
    {
        int result = 0;
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
                case "":
                    throw new ArgumentException("Operation cannot be empty.");
                case null:
                    throw new ArgumentNullException("Operation cannot be null.");
                default:
                    throw new ArgumentOutOfRangeException(operation, $"Operation {operation} does not exist");
            }
        }
        catch(OverflowException e)
        {
            throw new ArgumentException(e.ToString());
        }

        return $"{operand1} {operation} {operand2} = {result}";
    }

    private static int Division(int operand1, int operand2)
    {
        return operand1 / operand2;
    }

    private static int Multiplication(int operand1, int operand2)
    {
        checked
        {
            return operand1 * operand2;
        }
    }
    private static int Addition(int operand1, int operand2)
    {
        checked
        {
            return operand1 + operand2;
        }
    }

}