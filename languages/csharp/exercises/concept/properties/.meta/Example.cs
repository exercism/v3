using System;

public enum Units
{
    Kilograms
    ,Pounds
}
public class WeighingMachine
{
    private const float POUNDS_PER_KILOGRAM = 2.20462f;
    private float weight;

    public Units Units { get; set; } = Units.Kilograms;
    public float Weight
    {
        get { return Reduce(weight); }
        set
        {
            if (value < 0)
            {
                throw new ArgumentException("weight cannot be negative");
            }

            weight = value;
        }
    }

    public BritishWeight BritishWeight
    {
        get
        {
            float adjustedWeight = Reduce(weight);
            float weightInPounds = WeightInPounds(adjustedWeight);
            return new BritishWeight(weightInPounds);
        }
    }
    public float Reduction { set; private get; }
    private float Reduce(float weight) => weight * (100 - Reduction) / 100;
    private float WeightInPounds(float weight) => Units == Units.Kilograms ? weight * POUNDS_PER_KILOGRAM : weight;
}

public struct BritishWeight
{
    private const int POUNDS_PER_STONE = 14;
    private const float OUNCES_PER_POUND = 16f;

    public BritishWeight(float weightInPounds)
    {
        Stones = (int)weightInPounds / POUNDS_PER_STONE;
        Pounds = (int)Math.Floor(weightInPounds) - ((int)weightInPounds / POUNDS_PER_STONE) * POUNDS_PER_STONE;
        Ounces = (int)(OUNCES_PER_POUND * (weightInPounds - (int)weightInPounds));
    }

    public BritishWeight(int stones, int pounds, int ounces)
    {
        Stones = stones;
        Pounds = pounds;
        Ounces = ounces;
    }

    public int Stones { get; }
    public int Pounds { get; }
    public int Ounces { get; }
}
