public struct Currency
{
    private decimal value;
    private string unit;

    public Currency(decimal value, string unit)
    {
        this.value = value;
        this.unit = unit;
    }
}
