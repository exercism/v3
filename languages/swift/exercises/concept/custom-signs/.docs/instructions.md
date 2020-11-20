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

## 5. Compute the cost of a sign

Implement the function `costOf(sign: String) -> Int` which takes a string that holds the contents of the sign and returns the cost to create the sign, which is 2 dollars for each character in the sign plus a base price of 20 dollars.

```swift
costOf(sign: "Happy Birthday Grandma!")
// => 66
```
