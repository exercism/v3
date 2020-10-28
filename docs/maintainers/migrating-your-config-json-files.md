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

4. Remove the `"foregone"` property. The property will return once the Practice Exercises have been added.

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

## New keys
<!-- TODO: Properly document the new config.json format -->

Exercises may contain the following new keys:

- `name`: The title of the exercise if it differs from the titleised slug (optional)
