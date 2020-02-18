# Instructions

You are tasked with creating a program that will accept HTML Response Codes and output a value that's easily human readable.

### 1. Given a response code, display it in human language.

You are given a list of three basic codes that you should implement:
- 200 means `Success`
- 400 means `ClientError`
- 500 means `ServerError`

Define an `ErrrorCode` enum that will represent these three response codes, take note that no spaces are allowed. Codes are inscrutable, your users would favor something they can easily understand. Help your users by **implementing a method that will return the response as a string instead of just a number.**

### 2. Differentiate between successful and unsuccessful given more codes

You're given more codes to implement (You can't have spaces when defining possible values for an `enum`):
- 200 means `OK`
- 204 means `No Content`
- 301 means `Moved Permanently`
- 307 means `Temporary Redirect`
- 400 means `Bad Request`
- 404 means `Not Found`
- 500 means `Internal Server Error`
- 501 means `Not Implemented`

However, your users don't really care these technical descriptions of responses. They just want to know whether it's a successful request or not. For this, **implement a method that will return either the string "Successful Request" or "Not Successful"**

### 3. Use a switch-case statement to output response messages with spaces

Now that your users have validated your design, it's time to provide more detailed feedback for your support team. Instead of just returning whether the request is successful or not, your team wants to know the exact response message, complete with spaces.

We can do this with a series of `if` statements but this is a great time to learn about the `switch` [statement](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/switch). `switch` statements are great when you're just looking at the value of one variable and checking it against multiple possible values. You put that variable inside the `switch` statement, followed by the `case` statements with values you want to check it against. After your code for each statement, don't forget the `break` keyword! Here's a basic example:

```csharp
int number = 2;
switch (number) {
    case 1:
        Console.WriteLine("Case 1");
        break;
    case 2:
        Console.WriteLine("Case 2");
        break;
    default:
        Console.WriteLine("Default case");
        break; 
}
```

For your task, **implement a method to return the response message with spaces, given the response code**.

### Bonus. Combine these three methods to have a more detailed output

Implement a method that will output a string containing whether the request is successful or not, response code, and the response message. Example:

`"Successful Request - 301: Moved Permanently"`

## Sources
[HTML Response Codes](https://restfulapi.net/http-status-codes/)

[`switch` statement](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/switch)
