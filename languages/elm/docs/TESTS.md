Tests for an Elm project live in the `tests/` subdirectory of that project.
Each exercise tests suite can be run from that exercise root directory.

```bash
$ cd ~/exercism/elm/hello-world
$ elm-test
```

## Watch for Changes

Automatically run tests again when you save changes:

```bash
$ elm-test --watch
```

## Test Driven Development Flow

In most exercises, tests are already written in the file `tests/Tests.elm`.
Your goal is to make them pass by writing the code required
in the `src/<exercise>.elm` file.
Focus on the first failing test, fix it,
then remove or comment the next `skip <|` snippet in the tests file and repeat
until all tests pass.

## Feeling Stuck?

Your first goal is to get your code to compile, even with `Debug.todo` in it.
If you feel you don't know how to do something,
it is often useful to extract that piece into its own unimplemented function.

```elm
complexOperationOnMultipleStrings : List String -> List String
complexOperationOnMultipleStrings allStrings =
    List.map complexOperation allStrings


complexOperation : String -> String
complexOperation input =
    Debug.todo "Extracted operation into its own function"
```

Adding a type annotation on this extracted function
will both help your thinking and the compiler to give you better error messages.

## Cleaning up your code

Consider running [`elm-format`](https://github.com/avh4/elm-format) on your code before submitting it
and even setting it up in your text editor for automatic formatting on save.
