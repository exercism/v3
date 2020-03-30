using System;

public static class Badge
{

    public static int ComputeWidthPx(string? FirstName,
                                     string? MiddleName,
                                     string? LastName,
                                     int? fontSizePx)
    {
        var words = (FirstName==null?0:1)
            + (MiddleName==null?0:1)
            + (LastName==null?0:1);

        return (FirstName??"").Length*(fontSizePx??0)
            + (MiddleName??"").Length*(fontSizePx??0)
            + (LastName??"").Length*(fontSizePx??0)
            + (Math.Max(words-1, 0))*(fontSizePx??0);
    }

}
