In this exercise you'll be writing code to help a sign company create custom messages for their signs.

## 1. Create a set of useful strings

Define the following constant strings which will be used to create signs:

- `birthday`: This holds the value "Birthday"
- `valentine`: This holds the value "Valentine's Day"
- `anniversary`: This holds the string "Anniversary"

## 2. Create a set of useful characters

Define the following constant characters which will be used to create signs:

- `space`: This holds the value " " (a single space)
- `exclamation`: This holds the value "!"

## 3. Combine phrases to build up messages

Implement the function `buildSign(for: String, name: String) -> String`. This function takes one of the three strings you defined in the first task as the `for` parameter and a String holding someone's name as the `name` parameter and uses concatenation as well as the characters defined in task #2 to build up a phrase for a sign.

```swift
buildSign(for: birthday, name: "Otto")
// => "Happy Birthday Otto!"

buildSign(for: anniversary, name: "Valentina")
// => "Happy Anniversary Valentina!"
```

## 4. Build a graduation sign

Implement the function `graduationFor(name: String, year: Int) -> String` which takes a name as a string parameter and a year as a string parameter and uses string interpolation to create a phrase for a sign that uses a newline to separate the two lines of the message.

```swift
graduationFor(name: "Padma", year: 2020)
// => "Congratulations Padma!\nClass of 2020"
```

## 5. Count the number of lines in a sign's message

The signs that the messages go on can only hold up to four lines. Implement the function `numberOfLines(in: String) -> Int` that takes a string as input, iterates over the characters in the string to count the number of newline characters. Return the number of newlines plus 1 as the number of lines in the message.

## 6. Count the number of numbers in a sign's message

Because they are lower in stock, numbers cost more to put in a sign's message than letters. Implement a function `numberOfNumbers(in: String) -> Int` that takes a string as input, iterates over the characters in the string to count the number of characters which are numbers and return this count.
