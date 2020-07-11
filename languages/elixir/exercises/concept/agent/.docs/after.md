The choice to use the `Agent` module, or to use multiple processes at all, depends on the relationship to be modelled with the data. _Agent processes_ are useful when a shared state must be used by multiple processes.

It is often the better choice to avoid using a separate process and use an in-process variable to hold the state in a function. Care should also be taken from blindly treating them as a global variable, as they may be manipulated by other processes, creating race conditions, or untraceable errors.

If choosing to use an _agent process_, the goal should be to hold a simple state, encapsulating the calls to the _agent process_ within a module to organize the calls.

```elixir
# Some example planned to go here.
```

If an expensive job should be done in another process, a _task process_ (using Task) process should be considered. If it is determined that a process should hold state and perform behaviours, a _GenServer process_ (using `GenServer`) should be considered.

> `Task` and `GenServer` will be discussed in another exercise.
