[_Callbacks_ describe the pattern][wiki-callbacks] where a function receives a function as an argument to invoke when it arrives at a condition. The condition may be that its own work is done, or that an event has occurred. Let's look at a few examples of Callbacks:

## Asynchronous Code

JavaScript is designed to be single-threaded, non-blocking, and event-driven. Events and callbacks facilitates asynchronous code. Following a top-to-bottom code-execution, design choices are often made to allow the code to hand off to an _API_ to prevent the thread from being blocked. When the API finishes, a _callback_ is then invoked to react to the result of the _API_ call. The end result is non-blocking code that allows for code to run in the intended order.

```javascript
// A Synchronous Callback
function send(message) {
  return message
}

function submitExercise(callback) {
  submit()
  return callback('Success')
}

submitExercise(send) //
```

## Browser Events

<!--  -->

## Node.js Error Convention

In [Node.js][nodejs], [callbacks][node-callbacks] often follow a [similar convention][node-error-convention] for their arguments: The first argument receives an `Error` object or `null` if no error occurred, and the second and subsequent receive the data that the calling function is designed to send.

If an error occurs, the second and subsequent arguments may not be present, so don't depend on them.

```javascript
function operation(a, b, callback) {
  // Work

  if (/* an error occurs */) {
    return callback(new Error('An error occurred'))
  }

  // On success

  callback(null, data)
}

function callback(error, returnedData) {
  if (error) {
    // An error occured, handle it here.
    return
  }

  // No error occurred, continue on with the returned data.
}
```

You see this pattern often when dealing with asynchronous functions.

## Resources

- [Twilio: Asynchronous JavaScript: Understanding Callbacks][twilio-callbacks]
- [MDN: Callback function][mdn-callbacks]
- Node.js:
  - [Callbacks][node-callbacks]
  - [Callback Error Convention][node-error-convention]
- [Callback Hell][callback-hell]

[callback-hell]: http://callbackhell.com/
[edspresso-callbacks]: https://www.educative.io/edpresso/what-are-callbacks-in-javascript
[mdn-callbacks]: https://developer.mozilla.org/en-US/docs/Glossary/Callback_function
[nodejs]: https://www.nodejs.org
[node-callbacks]: https://nodejs.org/en/knowledge/getting-started/control-flow/what-are-callbacks/
[node-error-convention]: https://nodejs.org/en/knowledge/errors/what-are-the-error-conventions/
[twilio-callbacks]: https://www.twilio.com/blog/asynchronous-javascript-understanding-callbacks
[wiki-callbacks]: https://en.wikipedia.org/wiki/Callback_(computer_programming)
