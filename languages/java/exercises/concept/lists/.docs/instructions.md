In this exercise you'll be writing code to keep track of a list of programming languages you want to learn on Exercism.

You have nine tasks, which will all involve dealing with lists.

## 1. Create a new list

To keep track of the languages you want to learn, you'll need to create a new list.

Implement the static `Languages.NewList()` method that returns a new, empty list.

```java
Languages.NewList()
// => empty list
```

## 2. Define an existing list

Currently, you have a piece of paper listing the languages you want to learn: Java, Clojure and Go.

Please implement the static `Languages.GetExistingLanguages()` method to return the list.

```java
Languages.GetExistingLanguages();
// => {"Java", "Clojure", "Go"}
```

## 3. Add a new language to a list

As you explore Exercism and find more interesting languages, you want to add them to your list.

Implement the static `Languages.AddLanguage()` function to add a new language to the end of your list.

```java
Languages.AddLanguage(Languages.GetExistingLanguages(), "VBA");
// => {"Java", "Clojure", "Go", "VBA"}
```

## 4. Count the languages in the list

Counting the languages one-by-one is inconvenient.

Implement the static `Languages.CountLanguages()` method to count the number of languages on your list.

```java
Languages.CountLanguages(Languages.GetExistingLanguages())
// => 3
```

## 5. Check the last added language

You would also want to check which language you just added.

Implement the static `Languages.LastLanguage()` method that returns the last language from your list.

```java
Languages.LastLanguage(Languages.GetExistingLanguages());
// => Java
```

## 6. Check to see if a language is in the list

Implement the static `Languages.HasLanguage()` method to check if a language is present.

```java
Languages.HasLanguage(Languages.GetExistingLanguages(), "Go")
// => true
```

## 7. Remove Language

You now realize that you don't want to learn Clojure anymore. You want to remove it from your list.

Please implement the static `Languages.RemoveLangage()` method to remove a specified language from the list.

```java
Languages.RemoveLanguage(Languages.GetExistingLanguages(), "Clojure")
// => { "Java", "Go" }
```

## 8. Check if a language is unique

You were really intent on learning Java, and as such, you are not sure if you put it twice in your list.

Please implement the static `Languages.ensureUnique()` method to check if a language is present only once in your list.

```java
Languages.ensureUnique(Languages.getExistingLanguages(), "Java");
// => true
```

## 9. Find where a language is in your list

You remember adding Java to your list, but you don't remember at which place.

Please implement the static `Languages.positionOf()` method to know at which index a language appears first in your list.

```java
Languages.positionOf(Languages.getExistingLanguages(), "Java");
// => 0
```
