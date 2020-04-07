using System;

public static class Badge
{
    public static string Label(int id, string name, string? department)
    {
        var separator = " - ";
        return $"[{id}]{separator}{name}{separator}{department?.ToUpper() ?? "GUEST"}";
    }

    public static string PrintLabel(string label, int? maximumWidth)
    {
        if (maximumWidth == null)
        {
            return label;
        }
        else
        {
            var output = "";
            for(int i=0;i<label.Length;i+=maximumWidth.Value)
            {
                output += label.Substring(i, Math.Min(maximumWidth.Value, label.Length - i)) + "\n";
            }
            return output.Trim();
        }
    }
}
