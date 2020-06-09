using System;
using System.Text;

public static class Identifier
{
    public static string Clean(string identifier)
    {
        const char UNDERSCORE = '_';
        const char DASH = '-';
        const char ALPHA = 'α';
        const char OMEGA = 'ω';
        var sb = new StringBuilder();
        for (int i = 0; i < str.Length; i++)
        {
            char ch = str[i];
            if (Char.IsWhiteSpace(ch))
            {
                sb.Append(UNDERSCORE);
            }
            else if (Char.IsControl(ch))
            {
                sb.Append("CTRL");
            }
            else if (ch == DASH)
            {
                if (i + 1 < str.Length)
                {
                    sb.Append(Char.ToUpper(str[i + 1]));
                    i++;
                }
            }
            else if (Char.IsLetter(ch) && (ch < ALPHA || ch > OMEGA) || ch == UNDERSCORE)
            {
                sb.Append(ch);
            }
        }

        return sb.ToString();
    }
}
