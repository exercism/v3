In this exercise you'll be writing code to keep track of international dialling codes via an international dialing code dictionary.

The dictionary uses an integer for its keys (the dialing code) and a string (country name) for its values.

You have 11 tasks which involve dictionaries.

### 1. Create a New Dictionary

```csharp
Dictionaries.GetEmptyDictionary();
// empty dictionary
```

### 2. Create a Pre-populated Dictionary

Create a dictionary which contains the following 3 dialing codes: "United States of America" which has a code of 1, "Brazil" which has a code of 55 and "India" which has a code of 91:

```csharp
Dictionaries.GetExistingDictionary();
// 1 => "United States of America", 55 => "Brazil", 91 => "India"
```

### 3. Add a Country to an Empty Dictionary

Add "United Kingdom" with a dialing code of 44:

```csharp
Dictionaries.AddCountryToEmptyDictionary(44, "United Kingdom");
// 44 => "United Kingdom"
```

### 4. Add a Country to an Existing Dictionary

Add "United Kindom" with a dialing code of 44 to the dictionary created in task 2:

```csharp
Dictionaries.AddCountryToExistingDictionary(Dictionaries.GetExistingDictionary(),
  44, "United Kingdom");
// 1 => "United States of America", 44 => "United Kingdom", 55 => "Brazil", 91 => "India"
```

### 5. Get the Country Name Matching a Country Code

Check that a country with the country name for dialing code 55

```csharp
Dictionaries.GetCountryNameFromDictionary(
  Dictionaries.GetExistingDictionary(), 55);
// "Brazil"
```

### 6. Check that a Country Exists in the Dictionary

Check that a record for Brazil exists in the dictionary created in task 2

```csharp
Dictionaries.CheckCodeExists(Dictionaries.GetExistingDictionary(), 55);
// true
```

### 7. Attempt to Get Country Name for a Non-existent Country Code

Request the country name for a code that is not in the existing dictionary, e.g. 999. An empty string should be returned:

```csharp
Dictionaries.GetCountryNameFromDictionary(
  Dictionaries.GetExistingDictionary(), 999);
// string.Empty
```

### 8. Update a Country Name

Change the name of "United States of America" to "Les États-Unis":

```csharp
Dictionaries.UpdateDictionary(
  Dictionaries.GetExistingDictionary(), 1, "Les États-Unis");
// 1 => "Les États-Unis", 55 => "Brazil", 91 => "India"
```

### 9. Attempt to Update Name of Country that is not in the Dictionary

Try to change the name of a country with a code that is not in the dictionary e.g. 999. This should result in no change to the dictionary:

```csharp
Dictionaries.UpdateDictionary(
  Dictionaries.GetExistingDictionary(), 999, "Newlands");
// 1 => "United States of America", 55 => "Brazil", 91 => "India"
```

### 10. Remove a Country from the Dictionary

Remove India from the dictionary:

```csharp
Dictionaries.RemoveCountryFromDictionary(
  Dictionaries.GetExistingDictionary(), 91);
// 1 => "United States of America", 55 => "Brazil"
```

### 11. Find the Country with the Longest Name

Process the values in the dictionary to find the one with the longest name:

```csharp
Dictionaries.FindLongestCountryName(
  Dictionaries.GetExistingDictionary());
// "United States of America"
```
