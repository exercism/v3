### General

- An integer value can be defined as one or more consecutive digits.

### 1. Define the expected oven time in minutes

- You need to define a [binding][bindings].

### 2. Calculate the remaining oven time in minutes

- You need to define a [function][functions] with a single parameter.
- You can use the [mathematical operator for subtraction][operators] to subtract values.
- The last expression in a function is automatically returned from the function; you don't have to explicitly indicate which value to return.

### 3. Calculate the preparation time in minutes

- You need to define a [function][functions] with a single parameter.
- You can use the [mathematical operator for multiplicaton][operators] to multiply values.

### 4. Calculate the total working time in minutes

- You need to define a [function][functions] with two parameters.
- You can use the [mathematical operator for addition][operators] to add values.

[bindings]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/values/#binding-a-value
[functions]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/#remarks
[operators]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/symbol-and-operator-reference/arithmetic-operators