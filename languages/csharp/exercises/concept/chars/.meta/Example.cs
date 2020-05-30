using System;
using System.Text;
using Microsoft.VisualBasic.CompilerServices;

public static class CharUtils
{
    public static bool DetectInvalidCharComination(string str)
    {
        const char DASH = '-';
        const char GREATER_THAN = '>';
        for (int i = 1; i < str.Length - 1; i++)
        {
            if (str[i] == DASH && str[i + 1] == GREATER_THAN && str[i - 1] != DASH)
            {
                return true;
            }
        }

        return false;
    }

    public static string ToUpper(string str)
    {
        var sb = new StringBuilder();
        foreach (char ch in str)
        {
            sb.Append(Char.ToUpper(ch));
        }

        return sb.ToString();
    }

    public static string CleanIdentifier(string str)
    {
        const char UNDERSCORE = '_';
        var sb = new StringBuilder();
        bool nonDigitFound = false;
        foreach (char ch in str)
        {
            if (Char.IsWhiteSpace(ch))
            {
                continue;
            }
            else if (Char.IsControl(ch))
            {
                sb.Append("CTRL");
                nonDigitFound = true;
            }
            else if (!nonDigitFound && Char.IsDigit(ch))
            {
                sb.Append(UNDERSCORE);
                nonDigitFound = true;
            }
            else if (ch == UNDERSCORE || Char.IsLetterOrDigit(ch))
            {
                sb.Append(ch);
                nonDigitFound = true;
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
            if (!charInserted && Char.ToLower(ch).CompareTo(Char.ToLower((str[i - 1]))) > 0
                              && Char.ToLower(ch).CompareTo(Char.ToLower(str[i])) < 0)
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

    public static string InsertASCIICharacter(string str, char ch)
    {
        bool charInserted = false;
        var sb = new StringBuilder();
        if (string.IsNullOrEmpty(str))
        {
            return str;
        }
        sb.Append(str[0]);
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
