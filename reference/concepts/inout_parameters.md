# Inout parameters

Inout parameters are a type of [function][ref-functions] parameter that may have its value updated by the function, rather than returning a new value as a part of the return type. This is similar to, and may be optimized to the similar _call by reference_ concept, but may use value copying rather than direct referencing of the value's location.

> In-out parameters are passed as follows:
>
> 1. When the function is called, the value of the argument is copied.
> 2. In the body of the function, the copy is modified.
> 3. When the function returns, the copy’s value is assigned to the original argument.
>
> This behavior is known as _copy-in copy-out_ or _call by value result_.
>
> As an optimization, when the argument is a value stored at a physical address in memory, the same memory location may be used both inside and outside the function body. The optimized behavior is known as _call by reference_; it satisfies all of the requirements of the copy-in copy-out model while removing the overhead of copying. Write your code using the model given by copy-in copy-out, without depending on the call-by-reference optimization, so that it behaves correctly with or without the optimization.<sup>1</sup>

---

[1] In-Out Parameters, Swift.org. (2020). https://docs.swift.org/swift-book/ReferenceManual/Declarations.html#ID545 (accessed September 3, 2020).

[ref-functions]: ./functions.md
