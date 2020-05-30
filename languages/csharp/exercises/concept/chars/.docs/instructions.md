In this exercise you will implement a partial set of utility routines to help a developer
clean up identifier names and other text in a code base.

You have 5 tasks in implementing the utilities.

In all caseds the input string is guaranteed to be non-null.

### 1. Report the presence of "invalid" character sequences

Our CTO has a vehement dislike of the combination "->" ever since his days as an inadequate
C programmer. Any text found including that string must be reported.
Note that arrows such as "-->", "------>", etc. are allowed. It's just the C pointer that is objectionable.

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
- Replace control characters with the upper case string "CTRL"
- Replace any digits at the start of the string with underscores
- Ensure the first character is \_ or a unicode letter. @ is not permitted.
- Ensure that all other characters are unicode letters, decimal digits or \_.
  If other characters are found they should be omitted.
- Although other unicode characters, such as connecting or formatting characters,
  are permitted by the C# standard this shop does not allow them and
  they should be omitted.

```csharp
CharUtils.CleanIdentifier("9-abcĐ😀\0");
// => "_abcĐCTRL"
```

### 4. Insert character into a string

As part of a word game this final task requires you to design a utility method which allows
a letter to be inserted into a string. It should be inserted between "friendly"
characters in sort order. If no such pair of friendly characters exists
then the character should be appended to the string. Comparisons should be case insensitive.

```csharp
CharUtils.InsertChar("ႥႧ", 'Ⴆ');
// => "ႥႦႧ"
```

### 5. Insert an ASCII character into a string

Add a version of the utility with the same behavior as that of Task 4 but
which takes advantage of ASCII's simpler comparisons.
