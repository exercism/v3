### 1. Display the hit points

- Store the hit points as a [field][fields] on the `Character` class.
- [Define a constructor][constructor-syntax] on the `Character` class that takes the number of hit points. Also define constructors for the `Wizard` and `Warrior` classes that pass their character type's hit point to their base class' constructor.
- The `Character` class implicitly inherits from the `object` class, which [`ToString()` method you can override][override-tostring].

### 2. Allow attacking a character using a basic attack

- Implement the `Damage()` method on the `Wizard` and `Warrior` classes to return their base attack's damage.
- Abstract methods can be used in regular methods. At runtime, they will execute the derived class' logic.
- Update the hit points field of the target character parameter.

### 3. Allow wizards to use their special attack

- Add a field to the `Wizard` class to store if a spell was prepared and update this in the `PrepareSpell()` method.
- Add a conditional to the `Damage()` method to check if a spell was prepared and update the damage done accordingly.
- Remember that the spell effect only lasts for one attack.

### 4. Allow warriors to use their special attack

- Add a field to the `Warrior` class to store if a potion was drunk and update this in the `DrinkPotion()` method.
- Add a conditional to the `Damage()` method to check if a potion was drunk and update the damage done accordingly.
- Remember that the potion effect only lasts for one attack.

### 5. Check for stunned characters

- Check if the number of hit points is less than or equal to zero.

### 6. Stunned characters cannot do damage

- Add a conditional to the `Attack()` method that uses the `Stunned()` method to prevent any damage from being done.

[constructor-syntax]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/constructors#constructor-syntax
[instance-constructors]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/instance-constructors
[while]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/while
[fields]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/fields
[override-tostring]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/how-to-override-the-tostring-method
