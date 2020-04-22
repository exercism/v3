using System;

public static class Badge
{
    public static string Label(int? id, string name, string? department)
    {
        var idLabel = (id == null ? "" : $"[{id}] - ");
        return $"{idLabel}{name} - {department?.ToUpper() ?? "GUEST"}";
    }

    public static string PrintLabel(string label, int maximumWidth)
    {
        var output = "";
        for (int i = 0; i < label.Length; i += maximumWidth)
        {
            output += label.Substring(i,
                                      Math.Min(
                                               maximumWidth,
                                               label.Length - i)) + "\n";
        }
        return output.Trim();
    }

}
