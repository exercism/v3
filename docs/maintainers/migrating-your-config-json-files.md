# Migrating your config.json files

For v3, the existing `config.json` files will need to be updated. You can use the [C# config.json](../../languages/csharp/config.json) for reference.

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

4. Added concepts for all Practice Exercises

The existing exercises are temporarily removed from the `config.json` file. They will return as practice exercises once the concept exercises have been added.

More details will be added at a later stage.
