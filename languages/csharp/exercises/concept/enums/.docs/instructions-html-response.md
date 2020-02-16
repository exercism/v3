# Instructions

Work with [HTML Response Codes](https://restfulapi.net/http-status-codes/)

## Outline

1. Implement a method to return a string using a 3-value enum
2. Define more values to our enum, use if statement to return either "Successful Request" or "Not Successful"
3. Use switch statement to return a string with spaces, "Client Error", and "Server Error"

### 1. Define a simple enum for error codes

Define an `ErrrorCodes` enum that has three elements, take note that no spaces are allowed:

- `Success`
- `ClientError`
- `ServerError`

Next, implement a method that will return the corresponding error message