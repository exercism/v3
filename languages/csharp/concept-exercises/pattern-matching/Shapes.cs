public class Square
{
    public double Side { get; }

    public Square(double side)
    {
        Side = side;
    }
}

public class Circle
{
    public double Radius { get; }

    public Circle(double radius)
    {
        Radius = radius;
    }
}

public struct Rectangle
{
    public double Length { get; }
    public double Height { get; }

    public Rectangle(double length, double height)
    {
        Length = length;
        Height = height;
    }
}

public class Triangle
{
    public double Base { get; }
    public double SideOne { get; }
    public double SideTwo { get; }

    public Triangle(double @base, double sideOne, double sideTwo)
    {
        Base = @base;
        SideOne = sideOne;
        SideTwo = sideTwo;
    }
}