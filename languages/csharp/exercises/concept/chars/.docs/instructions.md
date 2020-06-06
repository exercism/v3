In this exercise you will implement a partial set of utility routines to help a developer
clean up identifier names.

In the first 4 tasks you will gradually build up the routine `Clean` A valid identifier comprises
zero or more letters and underscores.

The final task is a little more esoteric involving inserting a character into an identifier

In all cases the input string is guaranteed to be non-null.

### 1. Replace any spaces encountered with underscores

This also applies to leading and trailing spaces

```csharp
Identifier.Clean("my   Id");
// => "my___Id"
```

### 2. Replace control characters with the upper case string "CTRL"

```csharp
Identifier.Clean("my\0Id");
// => "myCTRLId",
```

### 3. Convert kebab-case to camel-case

An identifier such as my-object becomes myObject

```csharp
Identifier.Clean("à-ḃç");
// => "àḂç"
```

### 4. Ensure that an already valid identifier remains unchnged by the routine

Note that the `Clean` method should treat an empty string as valid

```csharp
Identifier.Clean("àḃç");
// => "àḃç"
```

### 5. Insert character into a string

Some code bases being cleaned up have identifiers that are considered too short.
Design a method which allows
a letter to be inserted into a string. As a kind of randomisation it should be inserted between "friendly"
characters in sort order. If no such pair of friendly characters exists
then the character should be appended to the string. Comparisons should be case insensitive.

```csharp
Identifier.AddFriendlyCharacter("ႥႧ", 'Ⴆ');
// => "ႥႦႧ"
```
