In this exercise you will implement a partial set of utility routines to help a developer
clean up identifier names and other text in a code base.

In the first 4 tasks you will gradually build up the routine `CleanIdentifier` A valid identifier comprises
zero or more letters and underscores.

The final task relates to a word game you are thinking of developing.

In all cases the input string is guaranteed to be non-null.

### 1. Ensure that an already valid identifier remains unchnged by the routine

Note that utility should treat an empty string as valid

```csharp
CharUtils.CleanIdentifier("àḃç");
// => "àḃç"
```

### 2. Replace any spaces encountered with underscores

This also applies to leading and trailing spaces

```csharp
CharUtils.CleanIdentifier("my   Id");
// => "my___Id"
```

### 3. Replace control characters with the upper case string "CTRL"

```csharp
CharUtils.CleanIdentifier("my\0Id");
// => "myCTRLId",
```

### 4. Convert kebab-case to camel-case

An identifier such as my-object becomes myObject

```csharp
CharUtils.CleanIdentifier("à-ḃç");
// => "àḂç"
```

### 5. Insert character into a string

As part of a word game this final task requires you to design a utility method which allows
a letter to be inserted into a string. It should be inserted between "friendly"
characters in sort order. If no such pair of friendly characters exists
then the character should be appended to the string. Comparisons should be case insensitive.

```csharp
CharUtils.InsertChar("ႥႧ", 'Ⴆ');
// => "ႥႦႧ"
```
