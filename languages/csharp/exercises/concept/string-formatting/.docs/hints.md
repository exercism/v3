## General

## 1. Display the couple's name separated by a heart

- Take a look at the discussion of [string interpolation][string-interpolation].

## 2. Display the couple's initials in a ascii art heart

- This [article][verbatim-strings] discusses verbatim strings literals.
- This [docunent][string-format] discusses `String.Format()`.

## 3. German exchange students should be made to feel at home with locale sensitive declarations.

- To work with interpolated strings view the documentation for [`FormattableString`][formattable-string].
- You will need to work with [`CultureInfo`][culture-info]. Note that it implements the [`IFormatProvider`][format-provider] interface.

[string-interpolation]: https://docs.microsoft.com/en-us/dotnet/csharp/tutorials/string-interpolation
[format-provider]: https://docs.microsoft.com/en-us/dotnet/api/system.iformatprovider?view=netcore-3.1
[custom-formatter]: https://docs.microsoft.com/en-us/dotnet/api/system.icustomformatter?view=netcore-3.1
[string-format]: https://docs.microsoft.com/en-us/dotnet/api/system.string.format?view=netcore-3.1#System_String_Format_System_String_System_Object_System_Object_System_Object_
[verbatim-strings]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/#regular-and-verbatim-string-literals
[culture-info]: https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=netcore-3.1
[formattable-string]: https://docs.microsoft.com/en-us/dotnet/api/system.formattablestring?view=netcore-3.1
