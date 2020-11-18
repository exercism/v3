# Actor Model

The _actor model_ is a computational model of concurrency where an _actor_ is the computational primitive. In response to messages sent by other _actors_, the _actor_ can: make local decisions, create more actors, send more messages, determine how to respond to the next message. They can manage their own private state, and can affect other _actors_ only by sending a message.<sup>1</sup>

## Exercises

### Take-A-Number Machine

This exercise deals with a machine that gives out consecutive numbers. The reference implementation (Elixir) teaches:

- How to start a process.
- How to send a message to a process.
- How to receive a message.

#### Implementations

| Track  | Exercise                           | Changes |
| ------ | ---------------------------------- | ------- |
| Elixir | [processes][implementation-elixir] | None    |

[implementation-elixir]: ../../languages/elixir/exercises/concept/take-a-number/.docs/introduction.md

---

[1] Actor model, Wikipedia. (2020). https://en.wikipedia.org/w/index.php?title=Actor_model&oldid=939106706 (accessed February 29, 2020).
