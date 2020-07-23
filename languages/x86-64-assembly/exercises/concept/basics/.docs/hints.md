## General

- You need to make the functions [visible to other files][global].
- The [NASM documentation][interfacing] describes how function arguments are passed and return values are retrieved.

## 1. Define the expected oven time in minutes

- There is an [instruction][mov] to store a value in a register.

## 2. Calculate the remaining oven time in minutes

- There is an [instruction][sub] for subtracting values.

## 3. Calculate the preparation time in minutes

- There is an [instruction][imul] for multiplying values.

## 4. Calculate the elapsed time in minutes

- You can [call][call] one of the other functions you've defined previously.
- There is an [instruction][add] for adding values.

[global]: https://www.nasm.us/xdoc/2.15.02/html/nasmdoc7.html#section-7.7
[interfacing]: https://www.nasm.us/xdoc/2.15.02/html/nasmdo12.html#section-12.3
[mov]: https://www.felixcloutier.com/x86/mov
[sub]: https://www.felixcloutier.com/x86/sub
[imul]: https://www.felixcloutier.com/x86/imul
[call]: https://www.felixcloutier.com/x86/call
[add]: https://www.felixcloutier.com/x86/add
