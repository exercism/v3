using System;

public class BirdCount
{
    private int[] counts;

    public BirdCount(int[] countsForLastSevenDays)
    {
        this.counts = countsForLastSevenDays;
    }

    public int Total()
    {
        var total = 0;

        foreach (var count in counts)
        {
            total += count;
        }

        return total;
    }

    public int BusyDays()
    {
        var days = 0;

        foreach (var count in counts)
        {
            if (count >= 5)
            {
                days++;
            }
        }

        return days;
    }

    public int Yesterday()
    {
        return counts[5];
    }

    public bool Exact(int count)
    {
        return Array.IndexOf(counts, count) != -1;
    }

    public static int[] LastWeek()
    {
        return new int[] { 0, 2, 5, 3, 7, 8, 4 };
    }
}