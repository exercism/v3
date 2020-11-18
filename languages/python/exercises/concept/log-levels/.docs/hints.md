## General

- [Python Docs: Enum](https://docs.python.org/3/library/enum.html)

## 1. Parse log level

- Use [string `split`](https://www.w3schools.com/python/ref_string_split.asp) to extract the log level from the message.
- With the extracted part of the string, access the enum member with `LogLevel(string)`.

## 2. Support unknown log level

- Check if the extracted part of the string is a value of the enum `LogLevel`.
- If the value does not match any of the enum member values, then return the Unknown member.

## 3. Convert log line to short format

- Match the log level to its code level (an integer), multiple solutions are possible: if statements, another enum or any other solution.
- Use string formatting to return the code level and the message.

## 4. Create an Alias

- Create the new alias member for Warn
- Return the newly created member

## 5. All Member Names and Values

- Iterate on all the members of the enum and return a list of tuple.
- The tuple can be constructed with `(item1, item2)`.
- The name and value of the enum can be accessed with `member.name` and `member.value`
