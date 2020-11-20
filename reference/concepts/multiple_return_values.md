# Multiple return values

> The primary use of output parameters is to return multiple values from a [function][ref-functions] […]. An important use of returning multiple values is to solve the semipredicate problem of returning both a value and an error status […]
>
> There are various alternatives to the use cases of output parameters.
>
> For returning multiple values from a function, an alternative is to return a tuple. Syntactically this is clearer if automatic sequence unpacking and parallel assignment can be used, as in Go or Python […]
>
> For returning a value of one of several types, a tagged union can be used instead; the most common cases are nullable types (option types), where the return value can be null to indicate failure. For exception handling, one can return a nullable type, or raise an exception.<sup>1</sup>

---

[1] Named parameters, Wikipedia. (2020). https://en.wikipedia.org/wiki/Parameter\_(computer\_programming)#Use (accessed September 3, 2020).

[ref-functions]: ./functions.md
