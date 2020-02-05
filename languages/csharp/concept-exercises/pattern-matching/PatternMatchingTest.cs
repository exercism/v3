using System;
using Xunit;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using Xunit.Sdk;

public class PatternMatchingTest
{
    private static readonly Square square = new Square(2);
    private static readonly Circle circle = new Circle(3);
    private static readonly Rectangle rectangle = new Rectangle(3,3);
    private static readonly Triangle triangle = new Triangle(2,2,2);
    [Fact] 
    public static void NoInput()
    {
        Assert.Equal(0,PatternMatch.Perimeter());
    }

    [Fact(Skip = "Skip")]
    public static void IncorrectInput()
    {
        Assert.Equal(0,PatternMatch.Perimeter("test"));
    }
    
    [Fact(Skip = "Skip")]
    public static void Square()
    {
       Assert.Equal(8,PatternMatch.Perimeter(square) ); 
    }

    [Fact(Skip = "Skip")]
    public static void Triangle()
    {
       Assert.Equal(6,PatternMatch.Perimeter(triangle)); 
    }
    [Fact(Skip = "Skip")]
    public static void Rectangle()
    {
        Assert.Equal(12,PatternMatch.Perimeter(rectangle));    
    }
    [Fact(Skip = "Skip")]
    public static void Circle()
    {
        Assert.InRange(PatternMatch.Perimeter(circle), 18, 19);
    }
}