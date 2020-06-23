This exercise has you analyze phone numbers.

You are asked to implement 2 features.

Phone numbers passed to the routines are guaranteed to be in the form
NNN-NNN-NNNN e.g. 212-515-9876 and non-null.

### 1. Analyze a phone number

Your analysis should return 3 pieces of data

1. An indication of whether the number has a New York dialing code ie. 212 as the first 3 digits
2. An indication of whether the number is fake having 555 in positions 5 to 7
3. The last 4 digits of the number.

Numbering above is 1 based.

Implement the method `PhoneNumber.Analyze()` to produce the phone number info.

```csharp
PhoneNumber.Analyze("631-555-1234");
// => (false, true, "1234")
```

### 2. Detect if a phone number is fake prefix code (555)

Implement the method `PhoneNumber.IsFake()` to detect whether the phone number is used using the phone number info produced in task 1.

```csharp
PhoneNumber.IsFake(PhoneNumbers.Analyze("631-555-1234"));
// => true
```

### 3. Compare the information about two phone numbers to detect duplicates

Implement the method `PhoneNumber.AreDuplicate()` to test equality.

```csharp
var inputPhoneNumberInfo = PhoneNumber.Analyze("212-502-1234");
var storedhoneNumberInfo = (true, false, "1234");
PhoneNumber.AreDuplicate(inputPhoneNumberInfo, storedhoneNumberInfo);
// => true
```
