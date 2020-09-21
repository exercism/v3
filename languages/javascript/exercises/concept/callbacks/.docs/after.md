[_Callbacks_ describe the pattern][wiki-callbacks] where a function receives a function as an argument to invoke when it arrives at a condition. The condition may be that its own work is done, or that an event has occurred. This is a useful pattern in JavaScript because it is designed as a single-threaded runtime where only one function call can be executed at a time. During execution, the runtime cannot respond to other events or continue execution until the function has returned. You might have noticed this on website when they seem to "freeze" or become unresponsive.

But many API calls (often I/O functions, and event listeners) use an asynchronous mechanism to place the [current function call][mdn-concurrency-stack] on the side until the work is complete. Upon completion, a callback function is placed on the [message queue][mdn-concurrency-queue] so that when the runtime is in between function calls, the messages are then processed by invoking the callback function.

## Synchronous Code

A synchronous callback where one exercise is executed after the other. The first exercise must return before the second exercise is submitted.

```javascript
function logMessage(message) {
  console.log(message)
}

function submitExercise(exercise, callback) {
  // Work is done here.
  return callback('Success')
}

submitExercise(exerciseOne, logMessage)
submitExercise(exerciseTwo, logMessage)
```

## Asynchronous Code

When an asynchronous function is invoked, there is no way to know for certain if it will return before the next instruction

```javascript
const value = asynchronousFunction()
console.log(value) // This may either be the result or undefined
```

So we can use callbacks to control the order of execution:

```javascript
function logMessage(message) {
  console.log(message)
}

function submitExerciseAsync(exercise, callback) {
  // Asynchronous work is done here

  return callback('Success')
}

// Either may invoke the callback first,
// depending on which finishes the asynchronous call first
submitExerciseAsync(exerciseOne, logMessage)
submitExerciseAsync(exerciseTwo, logMessage)
```

## Specific callback forms:

### Browser Events

_Event handlers_ are a common use case for callbacks in JavaScript. This often takes the form of browser events like `'onload'` or `'onclick'`, where a DOM object's `addEventListener` method then registers a callback to be invoked when a specified event occurs.

```javascript
document.addEventListener('onload' () => alert('The webpage has now been loaded'))
```

### Node.js Error Convention

In [Node.js][nodejs], [callbacks][node-callbacks] often follow a [similar convention][node-error-convention] for their arguments: The first argument receives an `Error` object or `null` if no error occurred, and the second and subsequent receive the data that the calling function is designed to send.

If an error occurs, the second and subsequent arguments may not be present, so don't depend on them.

```javascript
function operation(a, b, callback) {
  // Work ...

  if (/* an error occurs */) {
    return callback(new Error('An error occurred'))
  }

  // On success:
  callback(null, data)
}

function callback(error, returnedData) {
  if (error) {
    // An error occured, handle it here.
    return
  }

  // No error occurred, continue on with the returned data.
}

operation(1, 2, callback)
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
[mdn-concurrency-stack]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/EventLoop#stack
[mdn-concurrency-queue]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/EventLoop#queue
[nodejs]: https://www.nodejs.org
[node-callbacks]: https://nodejs.org/en/knowledge/getting-started/control-flow/what-are-callbacks/
[node-error-convention]: https://nodejs.org/en/knowledge/errors/what-are-the-error-conventions/
[twilio-callbacks]: https://www.twilio.com/blog/asynchronous-javascript-understanding-callbacks
[wiki-callbacks]: https://en.wikipedia.org/wiki/Callback_(computer_programming)
