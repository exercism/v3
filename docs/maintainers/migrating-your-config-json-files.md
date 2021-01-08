# Migrating your config.json files

For v3, the existing `config.json` files will need to be updated. You can use the [C# config.json](../../languages/csharp/config.json) for reference. You can re-use the existing `config.json`, but with the following modifications:

1. Add a version property:

```json
"version": 3
```

2. Add online editor settings:

```json
"online_editor": {
  "indent_style": "space",
  "indent_size": 2
}
```

- `indent_style`: set to `"tab"` or `"space"` to use hard tabs or soft tabs respectively.
- `indent_size`: a whole number defining the number of columns used for each indentation level and the width of soft tabs (when supported).

3. Convert the `"exercises"` array to an object with two properties:

```json
"exercises": {
  "concept": [],
  "practice": []
}
```

As can be seen, the existing exercises are temporarily removed from the `config.json` file. They will return as Practice Exercises once the Concept Exercises have been added. More details will be added at a later stage.

4. Move the `"foregone"` property to the `"exercises"` key:

```json
"exercises": {
  ...
  "foregone": [...]
}
```

5. Add a top-level `"concepts"` key, which is an array:

```json
"concepts": []
```

## Conversion script

All of the above changes can be applied automatically using the following [jq command][jq]:

```
jq 'del(.foregone)
    | .exercises = {concept: [], practice: []}
    | .online_editor = {indent_style: "...", indent_size: N}
    | to_entries
    | .[0:1] + [{"key":"version","value":3}] + .[1:]
    | from_entries
    ' config.json
```

[jq]: https://stedolan.github.io/jq/

## Concept exercises

Each concept exercise has the following keys:

- `slug`: the slug used to identify the concept exercise. The slug must be both unique per track _and_ use kebab-case.
- `name`: the human-friendly name of the concept exercise.
- `uuid`: the concept exercise's globally unique identifier.
- `concepts`: an array of concept slugs that this concept exercise unlocks when completed.
- `prerequisites`: an array of concept slugs that have to be unlocked before this concept exercise can be started.

There is also one optional key:

- `deprecated`: a boolean value that indicates if the concept exercise is deprecated. It should only be present when the exercise is actually deprecated.

This is an example of a concept exercise:

```json
{
  "slug": "bird-watcher",
  "name": "Bird Watcher",
  "uuid": "b6c532c9-1e89-4fbf-8f08-27f5befb5bb8",
  "concepts": ["arrays", "for-loops", "foreach-loops"],
  "prerequisites": ["booleans", "classes", "if-statements"]
}
```

## Practice exercises

Each practice exercise has the following keys:

- `slug`: the slug used to identify the practice exercise. The slug must be both unique per track _and_ use kebab-case.
- `name`: the human-friendly name of the practice exercise.
- `uuid`: the practice exercise's globally unique identifier.
- `prerequisites`: an array of concept slugs that have to be unlocked before this practice exercise can be started.
- `difficulty`: an integer in the range of 0-10 that describes how hard this practice exercise is to solve. It is not clear if we'll keep on using this property in the future, or if we'll deprecate it and determine the difficulty by other means.

There is also one optional key:

- `deprecated`: a boolean value that indicates if the practice exercise is deprecated. It should only be present when the exercise is actually deprecated.

This is an example of a practice exercise:

```json
{
  "slug": "rest-api",
  "name": "REST API",
  "uuid": "4c89b6a8-0e89-46c9-5a7e-31bfe6a57007",
  "prerequisites": ["strings"],
  "difficulty": 6
}
```

## Concepts

Each practice exercise has the following keys:

- `slug`: the slug used to identify the concept. The slug must be both unique per track _and_ use kebab-case.
- `name`: the human-friendly name of the concept.
- `uuid`: the concept's globally unique identifier.
- `blurb`: a short description of the concept.

This is an example of a concept:

```json
{
  "slug": "strings",
  "name": "Strings",
  "uuid": "d38a4651-09d2-47e5-8a18-9bcb6f3bfa0b",
  "blurb": "A `string` in C# is an object that represents immutable text as a sequence of Unicode characters (letters, digits, punctuation, etc.)."
}
```