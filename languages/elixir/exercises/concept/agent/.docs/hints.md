## General

- Review the documentation:
  - [Elixir - Getting started: Agent][getting-started-agent]
  - [Elixir Documentation: Agent][elixir-doc-agent]
- Watch this screencast:
  - [ElixirCasts: Introduction to Agents][elixircasts-agent]

## 1. Model the plot

- Review structs:
  - [Elixir - Getting started: Structs][getting-started-structs]
- The struct should have two required fields with no default values

## 2. Open the garden

- Review the Agent documentation
- The function to initialize the state of the _agent process_ must return the initial state.

## 3. List the registrations

- The Agent module contains many functions to obtain the current state of the _agent process_

## 4. Register plots to a person

- The Agent module contains functions to obtain and update the state of the _agent process_
- The functions generally require a function which transforms the state and returns a specific form.
- In order to keep track of the id for the next plot to assign, your _agent process_'s state may need to keep track of the plots and also the next id to use for a plot.

## 5. Release plots

- The Agent module contains functions to obtain and update the state of the _agent process_
- The functions generally require a function which transforms the state and returns the next state.

## 6. Get a registered plot

- The Agent module contains functions to obtain the state of the _agent process_
- Obtain the plot from, then handle the result to return the correct result

[elixircasts-agent]: https://elixircasts.io/intro-to-agents
[elixir-doc-agent]: https://hexdocs.pm/elixir/Agent.html
[getting-started-structs]: https://elixir-lang.org/getting-started/structs.html
[getting-started-elixir]: https://elixir-lang.org/getting-started/mix-otp/agent.html
