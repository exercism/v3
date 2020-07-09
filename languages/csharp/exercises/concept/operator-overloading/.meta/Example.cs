using System;

public struct Currency
{
    private decimal value;
    private string unit;

    public Currency(decimal value, string unit)
    {
        this.value = value;
        this.unit = unit;
    }

    public static bool operator ==(Currency @this, Currency other)
    {
        if (@this.unit != other.unit)
        {
            throw new ArgumentException();
        }

        return @this.value == other.value;
    }

    public static bool operator !=(Currency @this, Currency other)
    {
        return @this != other;
    }

    public static bool operator >(Currency @this, Currency other)
    {
        if (@this.unit != other.unit)
        {
            throw new ArgumentException();
        }

        return @this.value > other.value;
    }

    public static bool operator <(Currency @this, Currency other)
    {
        if (@this.unit != other.unit)
        {
            throw new ArgumentException();
        }

        return @this.value < other.value;
    }

    public static Currency operator +(Currency @this, Currency other)
    {
        if (@this.unit != other.unit)
        {
            throw new ArgumentException();
        }

        return new Currency(@this.value + other.value, @this.unit);
    }

    public static Currency operator -(Currency @this, Currency other)
    {
        if (@this.unit != other.unit)
        {
            throw new ArgumentException();
        }

        return new Currency(@this.value - other.value, @this.unit);
    }

    public static Currency operator *(Currency @this, decimal multiplier)
    {
        return new Currency(@this.value * multiplier, @this.unit);
    }

    public static Currency operator /(Currency @this, decimal divisor)
    {
        return new Currency(@this.value / divisor, @this.unit);
    }

    public static explicit operator double (Currency @this)
    {
        return (double) @this.value;
    }
}
