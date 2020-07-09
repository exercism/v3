## General

## 1. Display the couple's name separated by a heart

- Take a look at the discussion of [string interpolation][string-interpolation].

## 2. Display the couple's initials in a ascii art heart

- This [article][verbatim-strings] discusses verbatim strings literals.
- This [docunent][string-format] discusses `String.Format()`.

## 3. German exchange students should be made to feel at home with locale sensitive declarations.

- To work with interpolated strings view the documentation for [`FormattableString`][formattable-string].
- You will need to work with [`CultureInfo`][culture-info].  Note that it implements the [`IFormatProvider`][format-provider] interface.

[string-interpolation]: https://docs.microsoft.com/en-us/dotnet/csharp/tutorials/string-interpolation
[string-interpolation-in-depth]: https://weblog.west-wind.com/posts/2016/Dec/27/Back-to-Basics-String-Interpolation-in-C#
[string-interpolation-advanced]: https://www.meziantou.net/interpolated-strings-advanced-usages.htm
[formatting-types]: https://docs.microsoft.com/en-us/dotnet/standard/base-types/formatting-types
[standard-numeric-format-strings]: https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-numeric-format-strings
[custom-numeric-format-strings]: https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-numeric-format-strings
[standard-date-and-time-format-strings]: https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings
[custom-date-and-time-format-strings]: https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings
[string-builder]: https://docs.microsoft.com/en-us/dotnet/standard/base-types/stringbuilder
[format-provider]: https://docs.microsoft.com/en-us/dotnet/api/system.iformatprovider?view=netcore-3.1
[custom-formatter]: https://docs.microsoft.com/en-us/dotnet/api/system.icustomformatter?view=netcore-3.1
[string-format]: https://docs.microsoft.com/en-us/dotnet/api/system.string.format?view=netcore-3.1#System_String_Format_System_String_System_Object_System_Object_System_Object_
[verbatim-strings]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/#regular-and-verbatim-string-literals
[culture-info]: https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=netcore-3.1
