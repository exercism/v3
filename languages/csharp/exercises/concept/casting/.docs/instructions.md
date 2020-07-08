Our football club (first encountered in (cross-ref-tba)) is soaring in the leagues, and you have been invited to do some more work, this time on the security pass printing system.

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

All data passed to the security pass maker has been validated and is guaranteed to be non-null.

## 1. Get display name for a member of the support team as long as they are staff members

Please implement the `securityPassMaker.GetDisplayName()` method. It should return the value of the `Title` field instances of all classes derived from `Staff` and, otherwise, "Too Important for a Laminate".

```csharp
var lm = new LaminateMaker();
lm.GetDisplayName(new Manager());
// => "Too Important for a Laminate"
lm.GetDisplayName(new Physio());
// => "The Physio"
```

## 1. As for Task 1 except that if the staff member is a member of the security team the text "Priority Personnel" should be displayed after their title

Please modify the `securityPassMaker.GetDisplayName()` method. It should behave as in Task 1 except that if there is a security alert (constructor argument is `true`) and the staff member is a member of the security team (either of type `Security` or one of its derivatives) then the text " Priority Personnel" should be displayed after the title.

```csharp
var lm = new LaminateMaker();
lm.GetDisplayName(new Physio());
// => "The Physio"
var lm2 = new LaminateMaker();
lm2.GetDisplayName(new Security());
// => "Security Team Member Priority Personnel"
lm2.GetDisplayName(new SecurityJunior());
// => "Security Junior Priority Personnel"
```

## 3. As for Task 2 except that the Chairman feels that only the principal security team members should be designated as priority

Please modify the `securityPassMaker.GetDisplayName()` method. It should behave as in Task 2 except that the text " Priority Personnel" should not be displayed for instances of type `SecurityJunior`, `SecurityIntern` and `PoliceLiaison`.

```csharp
var lm2 = new LaminateMaker();
lm2.GetDisplayName(new Security());
// => "Security Team Member Priority Personnel"
lm2.GetDisplayName(new SecurityJunior());
// => "Security Junior"
```
