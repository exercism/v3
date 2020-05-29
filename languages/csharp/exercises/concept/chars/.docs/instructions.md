In this exercise you will implement a partial set of utility routines to help a developer
clean up identifier names and other text in a code base.

You have nnn tasks in implementing the utilities.

### 1. Report the presence of "invalid" character sequences

Our CTO has a vehement dislike of the combination "->" ever since his days as an inadequate
C programmer. Any text found including that string must be reported.

```csharp
CharUtils.DetectInvalidString("abc->def");
// => true
```

### 2. Convert a string to upper case

```csharp
CharUtils.ToUpper("aBc");
// => "ABC"
```

### 3. Ensure that a string is a valid C# identifier name

- Remove spaces
- Replace control characters with upper case "CTRL"
- Replace any digits at the start of the string with underscores
- Ensure the first character is _ or a unicode letter. @ is not permitted.
- Ensure that all other characters are unicode letters, decimal digits or _.
  If other characters are found they should be converted to _.
- Although other unicode characters, such as connecting or formatting characters,
  are permitted by the C# standard this shop does not allow them.

```csharp
CharUtils.CleanIdentifier("9-abcĐ😀\0");
// => "__abcĐ_CTRL"
```
