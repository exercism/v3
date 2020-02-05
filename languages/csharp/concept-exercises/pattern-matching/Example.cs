using System;

public static class Example
{
    public static double Perimeter(object shape = null)
    {
        var perimeter = shape switch
        {
            Square s => s.Side * 4,
            Circle c => (2 * Math.PI) * c.Radius,
            Rectangle r => 2 * (r.Width + r.Length),
            Triangle t => t.SideOne + t.SideTwo + t.SideThree,
            _ => 0
        };
        return perimeter;
    }
}
