Debugger gives a veiw of the results such that the captures for the match (`CaptureCollection`) appear to be a member of `Match` which it is not.

Groups are not always intuitive e.g. there is always a group 0 that you may not expect so using group names is good.

[regular expressions][regular-expressions] documentation describes regexes and the flavour built into the .NET libraries.
[Regex][regex] documentation describing the built-in library class.

[regular-expressions]: https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference
[regex]: https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=netcore-3.1
[so-groups-and-captures]: https://stackoverflow.com/questions/3320823/whats-the-difference-between-groups-and-captures-in-net-regular-expression
