using System;

public static class Example
{
    public static double Perimeter(object shape = null)
    {
        var perimeter = shape switch
        {
            Square s => s.Side * 4,
            Circle c => (2 * Math.PI) * c.Radius,
            Rectangle r => 2 * (r.Height + r.Length),
            Triangle t => t.Base + t.SideOne + t.SideTwo,
            _ => 0
        };
        return perimeter;
    }
}
