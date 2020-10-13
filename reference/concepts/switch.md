# Switch

> In computer programming languages, a switch statement is a type of selection control mechanism used to allow the value of a variable or expression to change the control flow of program execution via search and map.
>
> Switch statements function somewhat similarly to the if statement used in programming languages […] and exists in most high-level imperative programming languages[…] using such keywords as switch, case, select or inspect.
>
> Switch statements come in two main variants: a structured switch, as in Pascal, which takes exactly one branch, and an unstructured switch, as in C, which functions as a type of goto. The main reasons for using a switch include improving clarity, by reducing otherwise repetitive coding, and (if the heuristics permit) also offering the potential for faster execution through easier compiler optimization in many cases.<sup>1</sup>

## Exercises

### Sanitizing usernames

This exercise deals with sanitizing German names to only use ASCII characters.

- Keeping characters unchanged.
- Dropping characters.
- Changing 1 character to 2 characters (e.g. ö -> oe)

#### Implementations

| Track  | Exercise                           | Changes |
| ------ | ---------------------------------- | ------- |
| Elixir | [charlists][implementation-elixir] | None    |

---

[1] Switch statement, Wikipedia. (2020). https://en.wikipedia.org/wiki/Switch_statement (accessed September 3, 2020).

[ref-booleans]: ../types/boolean.md
[implementation-elixir]: ../../languages/elixir/exercises/concept/charlists/.docs/introduction.md
