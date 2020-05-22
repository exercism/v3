In this exercise you're working as an editor of a local newspaper. To have the titles on the front page grab the attention of readers, you apply some modifications.

You have four tasks, which will all transforming

## 1. Add a prefix

To make titles a bit more interesting, you sometimes add a prefix to a title. Implement the `prefix` function that takes the prefix as a `string` parameter. The function should return a new function that takes a `string` parameter and returns that `string` formatted as follows: `"<PREFIX> <TITLE>"

```fsharp
let addInterestingPrefix = prefix "[INTERESTING]"
addInterestingPrefix "New cinema opens."
// => "[INTERESTING] New cinema opens."
```

## 2. Cutoff the title

In general, you prefer shorter titles over longer ones. Implement the `cutoff` function that takes the `string` after which

and returns a function that takes a `string` parameter and returns that `string` formatted as follows: `"<PREFIX> <TITLE>"

```fsharp
let cutoffAtPeriod = cutoff "."
cutoffAtPeriod "New cinema opens. Community rejoices."
// => "New cinema opens."
```

## 2. Replace characters

Implement the `replace` function that takes a single `string` parameter containing the title's prefix, and returns a function that takes a `string` parameter and returns that `string` formatted as follows: `"<PREFIX> <TITLE>"

```fsharp
let headlinePrefix = prefix "[INTERESTING]"
headlinePrefix "New cinema opens."
// => "[INTERESTING] New cinema opens."
```
