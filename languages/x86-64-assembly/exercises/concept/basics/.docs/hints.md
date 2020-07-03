## General

- You need to declare the function as [`global`][global].
- The value in the `rax` register specifies the value returned by the function.

## 1. Define the expected oven time in minutes

- You can use the [`mov`][mov] instruction to store a value in a register.

## 2. Calculate the remaining oven time in minutes

- The function's parameter is in the `rdi` register.
- You can use the [`sub`][sub] instruction to subtract values.

## 3. Calculate the preparation time in minutes

- The function's parameter is in the `rdi` register.
- You can use the [`imul`][imul] instruction to multiply values.

## 4. Calculate the elapsed time in minutes

- The function's first parameter is in the `rdi` register.
- The function's second parameter is in the `rsi` register.
- You can use the [`call`][call] instruction to call other functions you've defined previously.
- You can use the [`add`][add] instruction to add values.

[global]: https://www.nasm.us/xdoc/2.15.02/html/nasmdoc7.html#section-7.7
[mov]: https://www.felixcloutier.com/x86/mov
[sub]: https://www.felixcloutier.com/x86/sub
[imul]: https://www.felixcloutier.com/x86/imul
[add]: https://www.felixcloutier.com/x86/add
[call]: https://www.felixcloutier.com/x86/call
