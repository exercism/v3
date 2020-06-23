This exercise addresses the parsing of log files.

After a recent security review you have been asked to clean up the organisation's archived log files.

All strings passed to the methods of guaranteed to be non-null and leading and trailing spaces are guaranteed to be removed.

### 1. Identify garbled log lines

You need some idea of how many log lines in your archive do not comply with current standards. You believe that a simple test reveals whether a log line is valid. To be considered valid a line should begin with one of the following strings:

- [TRC]
- [DBG]
- [INF]
- [WRN]
- [ERR]
- [FTL]

Implement `LogParser.IsValidLine()` to return `false` if a string is not valid otherwise `true`.

```csharp
var lp = new LogParser();
lp.IsValidLine("[ERR] A good error here");
// => true
lp.IsValidLine("Any old [ERR] text");
// => false
lp.IsValidLine("[BOB] Any old text");
// => false
```

### 2. Split the log line

A new team has joined the organization and you find their log files are using a strange separator for "fields". Instead of something sensible like a colon ":" they use a string such as "<--->" or "<=>" (because it's prettier) in fact any string that has a first character of "<" and a last character of ">" and any combination of the following "^\*=-" in between.

Implement `LogParser.SplitLogLIne()` that takes a line and returns an array of strings each of which contains a field.

```csharp
var lp = new LogParser();
lp.IsValidLine("Section 1<===>Section 2<^-^>Section 3");
// => {"Section 1", "Section 2", "Section 3"}
```

### 3. Count the number of lines containg a password

It is important to find any passwords included in a file. These will be dealt with automatically but the team needs to know how about passwords occurred in quoted text so that they can be examined manually.

Implement `LogParser.CountQuotedPasswords()` to identify lines where a password appears in quoted text.

The "password" string may be in upper or lower case or any combination.

Lines passed to the routine may or may not be valid as defined in task 1. We process them in the same way whether or not they are valid.

```csharp
string[] lines =
{
    string.Empty,
    "[INF] passWord",
    "\"passWord\"",
    "[INF] The message \"Please reset your passord\" was ignored by the user"
};
var lp = new LogParser();
lp.CountQuotedPasswords(lines);
// => {false, false, true, true}

```

### 4. Remove artifacts from log

You have found that some upstream processing of the logs has been scattering the text "end-of-line" followed by a line number (without an intervening space) throughout the logs.

Implement `LogParser.RemoveEndOfLineText()` to take a string and remove the end-of-line text and return a "clean" string.

Lines not containing end-of-line text should be returned unmodified.

Just remove the end of line string. Do not attempt to adjust the whitespaces.

```csharp
var lp = new LogParser();
lp.RemoveEndOfLineText("[INF] end-of-lline23033 Network Falure end-of-line27");
// => "[INF]  Network Failure "
```

### 5. Rewrite log to replace password with "xxxxxxxxx"

Implement `LogParser.RewriteLog()` to replace the actual password (rather than the text "password") with "xxxxxxxx" unless the password contains the string "pwassword" in which case the mask should be "**\*\*\*\***"

Lines with quoted passwords have already been removed and you process lines irrespective of whether they are valid (as per task 1).

Lines not containing a password should be returned unmodified.

Password is defined as the first string that follows the text password after a space.

Previous investigations have shown that no line contains more than one password.

```csharp
var lp = new LogParser();
lp.RewriteLogLines(new string[] {"my password theSecret"});
// => {"my password xxxxxxxx"}
lp.RewriteLogLines(new string[] {"my password password123"});
// => {"my password ********"}

```
