using System;

public static class Badge
{
    public static int WidthInPixels(string? text,
                                    int? fontSizePx)
    {

        return (text??"").Length*(fontSizePx??0);
    }

    public static string Label(string? firstName,
                               string? middleName,
                               string? familyName)
    {
        var output = "";

        output += firstName ?? "";
        output += middleName == null ? "" : (string.IsNullOrEmpty(output)?"":" ") + middleName;
        output += familyName == null ? "" : (string.IsNullOrEmpty(output)?"":" ") + familyName;

        return output;
    }
}
