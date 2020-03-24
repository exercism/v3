public static class Nullability
{

    public static bool IsNull(string s)
    {
        return s == null;
    }

    public static int TotalLength(string a, string b)
    {
        return a.Length + b.Length;
    }

    public static int TotalLengthSmart(string a, string b)
    {
        return (a??"").Length + (b??"").Length;
    }

}
