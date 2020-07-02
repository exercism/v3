using System;
using System.Collections.Generic;

public struct Coord
{
    public Coord(ushort x, ushort y)
    {
        X = x;
        Y = y;
    }

    public int X {get; }
    public ushort Y {get; }

    public bool Equals(Coord other)
    {
        return X == other.X && Y == other.Y;
    }

    public override bool Equals(object obj)
    {
        return obj is Coord other && Equals(other);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(X, Y);
    }
}

public struct Plot
{
    public Plot(Coord topLeft, Coord topRight, Coord bottomLeft, Coord bottomRight)
    {
        TopLeft = topLeft;
        TopRight = topRight;
        BottomLeft = bottomLeft;
        BottomRight = bottomRight;
        str = "mike";
    }

    public Coord TopLeft {get; }
    public Coord TopRight {get; }
    public string str { get; }
    public Coord BottomLeft {get; }
    public Coord BottomRight {get; }

    public bool Equals(Plot other)
    {
        return TopLeft.Equals(other.TopLeft) 
               && TopRight.Equals(other.TopRight) 
               && BottomLeft.Equals(other.BottomLeft) 
               && BottomRight.Equals(other.BottomRight);
    }

    public override bool Equals(object obj)
    {
        return obj is Plot other && Equals(other);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(TopLeft, TopRight, BottomLeft, BottomRight);
    }

    public int GetLongestSide()
    {
        return Math.Max(
            TopRight.X - TopLeft.X,
            Math.Max(BottomRight.X - BottomLeft.X,
                Math.Max(BottomRight.Y - TopRight.Y
                ,BottomLeft.Y - TopLeft.Y)));
    }
}


public class ClaimsHandler
{
    private ISet<Plot> plots = new HashSet<Plot>();
    private Plot lastClaim;

    public void StakeClaim(Plot plot)
    {
        lastClaim = plot;
        plots.Add(plot);
    }

    public bool IsClaimStaked(Plot plot)
    {
        return plots.Contains(plot);
    }

    public bool IsLastClaim(Plot plot)
    {
        return lastClaim.Equals(plot);
    }

    public Plot GetClaimWithLongestSide()
    {
        Plot longest = new Plot();
        foreach (Plot plot in plots)
        {
            if (plot.GetLongestSide() > longest.GetLongestSide())
            {
                longest = plot;
            }
        }

        return longest;
    }
}
