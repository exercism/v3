using System;

public static class Arrays
{
    //Fred's hours worked for the week.
    public static int[] EmployeeHoursPerDay() => new[]{0,9,4,8,8,9,8,0};

    public static int SumOfDays(int[] hoursPerDay = null)
    {
        var sum = 0;
        if (hoursPerDay != null)
        {
            foreach (var item in hoursPerDay)
            {
                sum += item;
            }
        }
        return sum;
    }

    public static double AvgOfDays(int[] hoursPerDay = null)
    {
        var sum = 0.0;
        var avg = 0.0;
        if (hoursPerDay != null)
        {
            foreach (var item in hoursPerDay)
            {
                sum += item;
            }
            avg = sum / hoursPerDay.Length;
        }
        return avg;
    }

    public static int FindFridayHours(int[] hoursPerDay = null)
    {
        var fridayHours = 0;
        if (hoursPerDay != null)
        {
            fridayHours = hoursPerDay[6];
        }
        return fridayHours;
    }
    
    public static int[] SortedDays(int[] hoursPerDay = null)
    {
        var sortedItems = new int[] {};
        if (hoursPerDay != null)
        {
            Array.Sort(hoursPerDay);
            Array.Reverse(hoursPerDay);
            sortedItems = hoursPerDay;
        }
        return sortedItems;
    }
}