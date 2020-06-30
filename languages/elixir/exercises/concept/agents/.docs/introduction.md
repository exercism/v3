The `Agent` module facilitates an abstraction for spawning processes and the _receive_, _send_ loop -- from here, we will call processes started using the `Agent` module _'agent processes'_. An _agent process_ might be chosen to represent a central shared state.

To start an unlinked _agent process_, `Agent` provides the `start/2` function:

```elixir
# Start an agent process with an initial value of an empty list
{:ok, agent_pid} = Agent.start(fn -> [] end)
```

Just like `Map` or `List`, `Agent` provides many functions for working with _agent processes_:

```elixir
# Get the current state of the agent process started previously
Agent.get(agent_pid, fn list -> list end)
# => []

# Add an element to the list
item = :element
Agent.update(agent_pid, fn list -> [item | list] end)
# => :ok
```

It is customary to organize and encapsulate all `Agent`-related functionality into a module for the domain being modeled.
