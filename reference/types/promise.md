# Promise

A write-once container which sets its value in the future. Similar to [Future][type-future]. It is a proxy for a value which may not be known at the time the promise is created and allows for asynchronous execution. In JavaScript, promises can be in one of three states<sup>1</sup>:
- Pending: the initial state before the promise has been completed
- Fulfilled (or Resolved): the success state after the operation is successfully completed
- Rejected: the error state signalling that the operation has failed

[type-future]: ./future.md

---

[1] Promise, MDN Web Docs. (2020). https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise (accessed June 3, 2020).
