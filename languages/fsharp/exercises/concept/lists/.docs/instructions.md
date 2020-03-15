In this exercise you'll be writing code to keep track of a list of programming languages you want to learn on Exercism.

You have six tasks, which will all involve dealing with lists.

### 1. Create a new list

To keep track of the languages you want to learn, you'll need to create an new list. Define a value that contains a new, empty list.

```fsharp
newList
// []
```

### 2. Define an existing list

Currently, you have a piece of paper listing the languages you want to learn: F#, Clojure and Haskell. Define a value to represent this list.

```fsharp
existingList
// ["F#"; "Clojure"; "Haskell"]
```

### 3. Add a new language to a list

As you explore Exercism and find more interesting languages, you want to add them to your list. Define a function to add a new language to the beginning of your list.

```fsharp
addLanguage "TypeScript" ["JavaScript"; "CoffeeScript"]
// ["TypeScript"; "JavaScript"; "CoffeeScript"]
```

### 4. Check which language was added last

You'll want to quickly check which language you added last. Define a function that returns the last added language on your list.

```fsharp
lastAddedLanguage ["Haskell"; "Python"]
// "Haskell"
```

### 5. Count the languages in the list

Counting the languages one-by-one is inconvenient. Define a function to count the number of languages on your list.

```fsharp
countLanguages ["C#"; "Racket"; "Rust"; "Ruby"]
// 4
```

### 6. Reverse the list

At some point, you realize that your list is actually ordered backwards! Define a function to reverse your list.

```fsharp
reverseList ["Prolog"; "C"; "Idris"; "Assembly"]
// ["Assembly"; "Idris"; "C"; "Prolog"]
```
