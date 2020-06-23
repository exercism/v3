using System.Text.RegularExpressions;

public class LogParser
{
    public bool IsMatch(string text)
    {
        const string searchArg = @"\[TRC\] | \[DBG\] | \[INF\] | \[ERR\] | \[WRN\] | \[FTL\]";
        return Regex.Match(text, searchArg, RegexOptions.IgnorePatternWhitespace).Success;
    }

    public string[] SplitLogLine(string text)
    {
        return Regex.Split(text, "<[*^=-]*>");
    }

    public bool[] AreQuotedPasswords(string[] lines)
    {
        bool[] results = new bool[lines.Length];
        var regex = new Regex(@"^.*""[^\\""]*password[^\\""]*"".*$", RegexOptions.IgnoreCase);
        int ctr = 0;
        for (int i = 0; i < lines.Length; i++)
        {
            results[i] = regex.IsMatch(lines[i]);
        }

        return results;
    }

    public string RemoveEndOfLineText(string line)
    {
        string pattern = @"end-of-line\d+";

        string str = Regex.Replace(line, pattern, string.Empty,
            RegexOptions.IgnoreCase | RegexOptions.IgnorePatternWhitespace);
        return str;
    }

    public string[] RewriteLogLines(string[] lines)
    {
        const string PREAMBLE = "preamble";
        const string PWTEXT = "pwtext";
        const string PW = "pw";
        const string SPACE = "space";
        const string POSTAMBLE = "postamble";

        var pattern = $@"
            ^
            (?<{PREAMBLE}>.*)          # any text
            (?<{PWTEXT}>password)      # the literal text - password
            (?<{SPACE}>\s+)
            (?<{PW}>\w*)               # the password itself
            (?<{POSTAMBLE}>.*)         # any text
            $
          ";

        string[] rewrites = new string[lines.Length];
        var regex = new Regex(pattern, RegexOptions.IgnoreCase | RegexOptions.IgnorePatternWhitespace);
        for (int i = 0; i < lines.Length; i++)
        {
            var matches = regex.Matches(lines[i]);
            if (matches.Count > 0)
            {
                var grps = matches[0].Groups;
                var mask = "xxxxxxxx";
                if (grps[PW].Value.ToLower().Contains("password"))
                {
                    mask = "********";
                }
                rewrites[i]
                  = $"{grps[PREAMBLE].Value}{grps[PWTEXT].Value}{grps[SPACE].Value}{mask}{grps[POSTAMBLE].Value}";
            }
            else
            {
                rewrites[i] = lines[i];
            }
        }

        return rewrites;
    }
}
