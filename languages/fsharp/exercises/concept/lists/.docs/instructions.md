In this exercise you'll be writing code to keep track of a list of programming languages you want to learn on Exercism.

You have six tasks, which will all involve dealing with lists.

## 1. Create a new list

To keep track of the languages you want to learn, you'll need to create an new list. Define the `newList` binding that contains a new, empty list.

```fsharp
newList
// => []
```

## 2. Define an existing list

Currently, you have a piece of paper listing the languages you want to learn: F#, Clojure and Haskell. Define the `existingList` binding to represent this list.

```fsharp
existingList
// => ["F#"; "Clojure"; "Haskell"]
```

## 3. Add a new language to a list

As you explore Exercism and find more interesting languages, you want to add them to your list. Implement the `addLanguage` function to add a new language to the beginning of your list.

```fsharp
addLanguage "TypeScript" ["JavaScript"; "CoffeeScript"]
// => ["TypeScript"; "JavaScript"; "CoffeeScript"]
```

## 4. Count the languages in the list

Counting the languages one-by-one is inconvenient. Implement the `countLanguages` function to count the number of languages on your list.

```fsharp
countLanguages ["C#"; "Racket"; "Rust"; "Ruby"]
// => 4
```

## 5. Reverse the list

At some point, you realize that your list is actually ordered backwards! Implement the `reverseList` function to reverse your list.

```fsharp
reverseList ["Prolog"; "C"; "Idris"; "Assembly"]
// => ["Assembly"; "Idris"; "C"; "Prolog"]
```

## 6. Check if list is exciting

While you love all languages, F# has a special place in your heart. As such, you're really excited about a list of languages if:

- The first on the list is F#.
- The second item on the list is F# and the list contain either two or three languages.

Implement the `excitingList` function to check if a list of languages is exciting:

```fsharp
excitingList ["Nim"; "F#"]
// => true
```
