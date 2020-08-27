using System;
using System.Linq.Expressions;

namespace ExerciseReport
{
    internal static class MyDebug
    {
        public static void Assert(Expression<Func<bool>> expr, string message)
        {
            var fun = expr.Compile();
            if (!fun())
            {
                throw new InvalidOperationException($"{message}: {expr}");
            }
        }
    }
}