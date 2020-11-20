# Guard

> In computer programming, a guard is a [boolean expression][ref-booleans] that must evaluate to true if the program execution is to continue in the branch in question.
>
> Regardless of which programming language is used, _guard clause_, _guard code_, or _guard statement_, is a check of integrity preconditions used to avoid errors during execution. A typical example is checking that a reference about to be processed is not null, which avoids null-pointer failures. Other uses include using a boolean field for idempotence (so subsequent calls are nops), as in the dispose pattern. The guard provides an early exit from a subroutine, and is a commonly used deviation from structured programming, removing one level of nesting and resulting in flatter code: replacing if guard { ... } with if not guard: return; ....<sup>1</sup>

---

[1] Guard (computer science), Wikipedia. (2020). https://en.wikipedia.org/wiki/Guard\_(computer\_science) (accessed September 3, 2020).

[ref-booleans]: ../types/boolean.md
