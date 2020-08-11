The introduction of the _csharp-exercise-report_ action creates two new files and changes the contents of another in _v3/languages/csharp/reference_:

- _exercises.json_ to bring together data about completed and proposed exercises.
- _exercise_errors.json_ to list inconsistencies between the original proposed concepts, design.md documents for completed exercises and the actual exercises in the track.
- _README.md_ is now generated entirely from _exercises.json_ and the _design.md_ docs of completed exercises as part of the action.

The action is triggered by a push of _csharp/reference/exercises.json_ or any _csharp/\*\*/.meta/design.md_.

There are 3 main activities:

- The action reads _exercises.json_ and all the _design.md_ files. The two sets of data are merged with the learning objectives taken from the _Concepts_ section of the _design.md_ and the rest is taken from the _exercises.json_ file. Preexisting learning objectives in the exercise file are deleted.

- _README.md_ is then generated based on the newly produced from the merged data above.

* A list of inconsistencies and other errors is written replacing _exercises-errors.json_.
