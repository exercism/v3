In this exercise you're playing a role-playing game named "Wizard and Warriors," which allows you to play (unsurprisingly) as a wizard or a warrior.

The wizard and warrior have some common data and behavior:

- They have a number of hit points.
- They can attack, which reduces the number of hit points of the other character.
- If the number of hit points is less than or equal to zero, they are stunned and cannot do any more damage.

There are also some differences between the two character types.

|                    | Warriors          | Wizards             |
| ------------------ | ----------------- | ------------------- |
| Initial hit points | 30                | 20                  |
| Basic attack       | 6                 | 3                   |
| Special attack     | 10 (potion drunk) | 12 (spell prepared) |

Drinking a potion (warrior) or preparing a spell (wizard) only makes the next attack a special attack. Subsequent attacks will be basic attacks, unless a new potion is drunk or a new spell is prepared.

You have six tasks that work with warriors and wizard characters.

### 1. Display the hit points

Override the `ToString()` method on the `Character` class to display the number of hit points of a character:

```csharp
var warrior = new Warrior();
warrior.ToString();
// => "HP: 30"
```

### 2. Allow attacking a character using a basic attack

Implement the `Warrior.Damage()` and `Wizard.Damage()` methods to return the damage done by their character type's basic attack. Next, implement the `Character.Attack()` method that takes the character to attack as its parameter and decrease the attacked character's hit points with the attacking character's damage.

```csharp
var warrior = new Warrior();
var wizard = new Wizard();
wizard.Attack(warrior);
warrior.ToString();
// => "HP: 27"
```

### 3. Allow wizards to use their special attack

Implement the `Wizard.PrepareSpell()` method to allow a wizard to prepare their spell in advance, making their next attack a special attack. Update the `Wizard.Damage()` method to return the special attack damage if a spell has been prepared.

```csharp
var warrior = new Warrior();
var wizard = new Wizard();

wizard.PrepareSpell();
wizard.Attack(warrior);
warrior.ToString();
// => "HP: 18"
```

### 4. Allow warriors to use their special attack

Implement the `Warrior.DrinkPotion()` method to allow a warrior to drink their potion, making their next attack a special attack. Update the `Warrior.Damage()` method to return the special attack damage if a potion has been drunk.

```csharp
var warrior = new Warrior();
var wizard = new Wizard();

warrior.DrinkPotion();
warrior.Attack(wizard);
wizard.ToString();
// => "HP: 10"
```

### 5. Check for stunned characters

Implement the `Character.Stunned()` method to return `true` if the character's hit points are less than or equal to zero:

```csharp
var warrior = new Warrior();
var wizard = new Wizard();
warrior.Attack(wizard);
warrior.Attack(wizard);
warrior.Attack(wizard);
warrior.Attack(wizard);
wizard.Stunned();
// => true
```

### 6. Stunned characters cannot do damage

Update the `Character.Attack()` method so that a stunned character cannot cause damage:

```csharp
var warrior = new Warrior();
var wizard = new Wizard();
warrior.Attack(wizard);
warrior.Attack(wizard);
warrior.Attack(wizard);
warrior.Attack(wizard);

wizard.Attack(warrior);
warrior.ToString();
// => "HP: 30"
```
