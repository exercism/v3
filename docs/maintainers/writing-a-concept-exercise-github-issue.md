# Writing a Concept Exercise GitHub issue

The GitHub issues we create for Concept Exercises need to contain quite a lot of information for an implementer to be able to work from.

We have provided an [Issue Template](https://github.com/exercism/v3/issues/new?assignees=&labels=status%2Fhelp-wanted%2C+type%2Fnew-exercise&template=implement-concept-exercise.md&title=%5B%3CLANG%3E%5D+Implement+new+Concept+Exercise%3A+%3CSLUG%3E) that needs to be filled in for each exercise. Part of that template requires a guide on how to implement the Concept Exercise for the specific track.

To help with that, each track should create a standard "Implementation Guide", which can be copy+pasted into the new issue. That file should be stored in `languages/$SLUG/reference/implementing-a-concept-exercise.md`

To make this as straightforward as possible for you, we have provided:

- [The C# implementation file](../../languages/csharp/reference/implementing-a-concept-exercise.md) to be clear on how this specific section should look.
- [A base file](./generic-how-to-implement-a-concept-exercise.md) for you to work from.
