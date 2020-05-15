### 1. Creating a Warrior or Wizard

- [Define constructors][constructor-syntax] on the `Wizard` and `Warrior` classes that pass their character type to the [base class' constructor][instance-constructors].

### 2. Describe a character

- Store the character type as a [field][fields] on the `Character` class.
- The `Character` class implicitly inherits from the `object` class, which [`ToString()` method you can override][override-tostring].

### 3. Make characters not vulnerable by default

- Implement the `Vulnerable()` method in the `Character` class to always return `false`.

### 4. Allow Wizards to prepare a spell

- Add a field to the `Wizard` class to store if a spell was prepared and update this in the `PrepareSpell()` method.
- The spell should start out as not being prepared.

### 5. Make Wizards vulnerable when not having prepared a spell

- Override the `Vulnerable()` method in the `Wizard` class to make Wizards vulnerable if they haven't prepared a spell.

### 6. Calculate the damage points for a Wizard

- Use a [conditional statement][if-else] to return the the damage points, taking into account the value of the prepare spell field.

### 7. Calculate the damage points for a Warrior

- You can call a method on the passed `Character` instance to determine its vulnerability.
- Use a [conditional statement][if-else] to return the the damage points, taking into account the vulnerability of the target.

[constructor-syntax]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/constructors#constructor-syntax
[instance-constructors]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/instance-constructors
[fields]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/fields
[override-tostring]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/how-to-override-the-tostring-method
[if-else]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/if-else
