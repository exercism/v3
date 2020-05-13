In this exercise you'll be writing code to keep track of international dialling codes via an international dialing code dictionary.

The dictionary uses an integer for its keys (the dialing code) and a string (country name) for its values.

You have nnn tasks which involve dictionaries.

### 1. Create a New International Dialing Code Dictionary

```csharp
Dictionaries.GetEmptyDictionary();
// empty dictionary
```

### 2. Create a Pre-populated International Dialing Code Dictionary

Create a dictionary which contains the following 3 dialing codes: "United States of America" which has a code of 1, "Brazil" which has a code of 55 and "India" which has a code of 91

```csharp
Dictionaries.GetExistingDictionary();
// 1 => "United States of America", 55 => "Brazil", 91 => "India"
```

### 3. Add A Country to an Empty International Dialing Code Dictionary

Add "United Kingdom" with a dialing code of 44 

```csharp
Dictionaries.AddCountrryToEmptyDictionary(44, "United Kingdom");
// 44 => "United Kingdom"
```

### 4. Add A Country to an Existing International Dialing Code Dictionary

Add "United Kindom" with a dialing code of 44 to the dictionary created in task 2.

```csharp
Dictionaries.AddCountryToExistingDictionary(Dictionaries.GetExistingDictionary,
  44, "United Kingdom");
// 1 => "United States of America", 44 => "United Kingdom", 55 => "Brazil", 91 => "India"
```
