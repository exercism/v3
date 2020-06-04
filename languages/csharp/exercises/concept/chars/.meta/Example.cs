using System;
using System.Text;

public static class CharUtils
{
    public static string CleanIdentifier(string str)
    {
        const char UNDERSCORE = '_';
        const char DASH = '-';
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
            else if (ch == UNDERSCORE || Char.IsLetter(ch))
            {
                sb.Append(ch);
            }
        }

        return sb.ToString();
    }

    public static string InsertCharacter(string str, char ch)
    {
        bool charInserted = false;
        var sb = new StringBuilder();
        if (!string.IsNullOrEmpty(str))
        {
            sb.Append(str[0]);
        }
        for (int i = 1; i < str.Length; i++)
        {
            if (!charInserted && Char.ToLower(ch) > Char.ToLower(str[i - 1])
                              && Char.ToLower(ch) < Char.ToLower(str[i]))
            {
                sb.Append(ch);
                charInserted = true;
            }

            sb.Append(str[i]);
        }

        if (!charInserted)
        {
            sb.Append(ch);
        }
        return sb.ToString();
    }
}
