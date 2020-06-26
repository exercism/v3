You are developing a system to help a wwb site's journalists reporting on EPL football/soccer matches. Data is received from a variety of sources and piped into a single stream after being cleaned up.

### 1. Output descriptions of the players based on their shirt number

Your website only covers teams that play a 4-3-3 formation and has never agreed with the 1965 change to the rules allowing for substitutions never mind enlarged squads.

The player descriptions are as follows:

```
1 -> "goalie"
2 -> "left back"
3 & 4 "part of the back 4"
...
```

Implement the static `PlayAnalyzer.OnField()` method to output a player description based on their shirt number.

```csharp
PlayAnalizer.AnalyzeOnField(10);
// => "striker"
```

### 2. Raise an alert if an unknown shirt number is encountered.

<< gonna have an exception here despite previously preaching about excluding them from normal control flow >>

### 3. Extend the coverage to include off field activity

Implement the `PlayAnalyzer.OffField()` method to output description of activities and characters around the field of play.

You receive a stream of data that has been cleaned. Your task is to analyse it and output appropriate text to help the journalists.

The data comprises:

- shirt numbers (any integer) -> text as per on field analysis
- free form text (any string) -> the text unchanged
- incidents in play (enum Incident) -> "RedCard", "Fowl" etc.
- managers (objects of type Manager) -> "the manager"

```csharp
PlayAnalizer.OffField(Incident.RedCard);
// => "RedCard"
PlayerAnalyzer.OffField(new Manager());
// => "the manager"
```

### 4. Where the manager has a nick name we want that output instead of "the manager"

Modify the `PlayAnalyzer.OffField()` method to output any nick name such as "The Special One" <<I'm not sure how well EPL jokes will be understood internationally but I expect people will get the idea>>
