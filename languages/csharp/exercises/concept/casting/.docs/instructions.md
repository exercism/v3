Our football club (first encountered in (cross-ref-tba)) is soaring in the leagues, and you have been invited to do some more work, this time on the laminate printing system.

The class hierarchy of the backroom staff is as follows

```
Team Support (interface)
    |
    - Chairman
    |
    - Manager
    |
    - Staff (abstract)
        |
        - Physio
        |
        - OffensiveCoach
        |
        - GoalKeepingCoach
        |
        - Security
            |
            - SecurityJunior
            |
            - SecurityIntern
            |
            - PoliceLiaison


```

A complete implementation of the hierarchy is provide as part of the source.

All data passed to the laminate maker has been validated and is guaranteed to be non-null.

## 1. Get display name for a member of the support team as long as they are staff members

Please implement the `laminateMaker.GetDisplayName()` method. It should return the value of the `Title` field instances of all classes derived from `Staff` and, otherwise, "Too Important for a Laminate".

```csharp
var lm = new LaminateMaker(false);
lm.GetDisplayName(new Manager());
// => "Too Important for a Laminate"
lm.GetDisplayName(new Physio());
// => "The Physio"
```

## 1. As for Task 1 except that if the staff member is a member of the security team the text "Priority Personnel" should be displayed after their title

Please modify the `laminateMaker.GetDisplayName()` method. It should behave as in Task 1 except that if there is a security alert (constructor argument is `true`) and the staff member is a member of the security team (either of type `Security` or one of its derivatives) then the text " Priority Personnel" should be displayed after the title.

```csharp
var lm = new LaminateMaker(false);
lm.GetDisplayName(new Physio());
// => "The Physio"
lm.GetDisplayName(new Security());
// => "Security Team Member"
var lm2 = new LaminateMaker(true);
lm2.GetDisplayName(new Security());
// => "Security Team Member Priority Personnel"
lm2.GetDisplayName(new SecurityJunior());
// => "Security Junior Priority Personnel"
```

## 3. As for Task 2 except that the Chairman feels that only the principal security team members should be designated as priority

Please modify the `laminateMaker.GetDisplayName()` method. It should behave as in Task 2 except that the text " Priority Personnel" should not be displayed for instances of type `SecurityJunior`, `SecurityIntern` and `PoliceLiaison`.

```csharp
var lm2 = new LaminateMaker(true);
lm2.GetDisplayName(new Security());
// => "Security Team Member Priority Personnel"
lm2.GetDisplayName(new SecurityJunior());
// => "Security Junior"
```

## 4. Convert the players shirt numbers for use by the laminate maker

Implement the `LaminateMaker.ConvertShirtNum()` method. This takes gthe shirt number as an unsigned long and converts it to an unsigned int. If the routine can't handle the shirt number then 0 should be returned.

```csharp
var lm = new LaminateMaker(false);
ln.ConvertShirtNum(21);
// => 21
ln.ConvertShirtNum(uint.MaxValue + 22);
// => 0
```
