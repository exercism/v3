x86-64 assembly is a low-level language. In assembly there are no variables,
instead we use registers to store values. Some registers have a special purpose
such as returning a value from a function, or passing function arguments. To
store a value in a register, we use the `mov` instruction:

```nasm
mov rax, 42  ; rax = 42
```

An assembly program is divided into sections. A section is basically a range of
addresses. The text section contains our executable instructions and is
declared as follows:

```nasm
section .text
```

A function is a set of instructions that perform a specific task. A function
declaration consists of a label with the name of the function, the instructions
that define the function, and the return instruction. The following declares a
function called `foo`, which returns the value 42:

```nasm
foo:
  mov rax, 42
  ret
```

The value in the `rax` register specifies the value returned by the function.

To change the visibility of a function, and be able to call it from any file in
our program we use the `global` keyword:

```nasm
global foo
```

When a function is called, the first argument is stored in the `rdi` register,
and the second argument is stored in the `rsi` register. Here's an example of a
function that takes a single argument and returns it, also known as an identity
function:

```nasm
global identity
identity:
  mov rax, rdi
  ret
```

To add values, we can use the `add` instruction. It takes two operands, a
source operand (first operand), and a destination operand (second operand),
adds them together, and stores the result in the destination operand. Here's an
example of a function that takes two arguments, adds them together, and returns
the result:

```nasm
global sum
sum:
  mov rax, rdi
  add rax, rsi  ; rax += rsi
  ret
```

To subtract values we use the `sub` instruction. It works just like the `add`
instruction except it subtracts the two operands.

To call a function, we use the `call` instruction. For example, to call our
`sum` function with the arguments 3 and 5, we would do the following:

```nasm
mov rdi, 3  ; First argument in rdi
mov rsi, 5  ; Second argument in rsi
call sum
; The rax register now contains the value 8
```
