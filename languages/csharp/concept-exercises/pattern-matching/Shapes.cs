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
    public double Width { get; }

    public Rectangle(double length, double width)
    {
        Length = length;
        Width = width;
    }
}

public class Triangle
{
    public double SideOne { get; }
    public double SideTwo { get; }
    public double SideThree { get; }

    public Triangle(double sideOne, double sideTwo, double sideThree)
    {
        SideOne = sideOne;
        SideTwo = sideTwo;
        SideThree = sideThree;
    }
}
