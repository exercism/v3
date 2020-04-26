To copy a value into a register we use the `mov` instruction:

```nasm
mov rdx, 10  ; Copy 10 into register rdx
```

A function can have zero or more parameters. The first parameter is passed in the `rdi` register, the second in the `rsi` register. To return a value, place it in the `rax` register. Use the `global` keyword to make the function accessible from other files.

```nasm
global add
add:
    mov rax, rdi  ; Copy first parameter into return register
    add rax, rsi  ; Add second parameter and return register. Store result in return register
    ret           ; Return to caller
```

Invoking a function is done using the `call` instruction. Put the first argument in the `rdi` register, and the second in the `rsi` register.

```nasm
mov rdi, 1  ; Copy 1 into first argument
mov rsi, 2  ; Copy 2 into second argument
call add    ; Invoke add function
```
