x86-64 assembly is a low-level language. In assembly there are no variables,
instead we use registers to store values. There are 16 general purpose 64-bit
registers, `rax`, `rbx`, `rcx`, `rdx`, `rsp`, `rbp`, `rsi`, `rdi`, and `r8`
through `r15`. The registers can also be accessed as 32-bit, 16-bit, or 8-bit.
For example the low 32-bits of `rax` can be accessed by `eax`, the low 16-bits
by `ax`, and the low 8-bits by `al`. The first four registers can even be
accessed by the high 8-bits of a 16-bit register.

Illustration of how the bits are accessed for the `rax` register:

```
+--------+---------------------------------------+
| 64-bit |                  rax                  |
+--------+-------------------+-------------------+
| 32-bit |                   |        eax        |
+--------+-------------------+---------+---------+
| 16-bit |                             |    ax   |
+--------+-----------------------------+----+----+
| 8-bit  |                             | ah | al |
+--------+-----------------------------+----+----+
```

Summary of how registers are identified for different access modes:

```
+----------+-------+-------+-------+-------+-----+-----+-----+-----+-----+
| 64-bit   |   rax |   rbx |   rcx |   rdx | rsp | rbp | rsi | rdi | rx  |
+----------+-------+-------+-------+-------+-----+-----+-----+-----+-----+
| 32-bit   |   eax |   ebx |   ecx |   edx | esp | ebp | esi | edi | rxd |
+----------+-------+-------+-------+-------+-----+-----+-----+-----+-----+
| 16-bit   |    ax |    bx |    cx |    dx |  sp |  bp |  si |  di | rxw |
+----------+-------+-------+-------+-------+-----+-----+-----+-----+-----+
| 8-bit    | ah/al | bh/bl | ch/cl | dh/dl | spl | bpl | sil | dil | rxb |
+----------+-------+-------+-------+-------+-----+-----+-----+-----+-----+
```

Some registers have a special purpose such as returning a value from a
function, or passing function arguments. To store a value in a register, we use
the `mov` instruction:

```nasm
mov rax, 42  ; rax = 42
```

An assembly program is divided into sections. The text section holds the
executable instructions of a program and is declared as follows:

```nasm
section .text
```

A function is a set of instructions that perform a specific task. A function
declaration consists of a label with the name of the function, the instructions
that define the function, and the return instruction. The following declares a
function called `foo`, which returns the value 42:

```nasm
foo:
  mov eax, 42
  ret
```

The value in the `rax` register specifies the value returned by the function.
Note that we are writing to the 32-bit part of `rax` here (`eax`). Writing to a
32-bit register also clears the upper bits, so `mov eax, 42` is the same as
`mov rax, 42`. This is not true for 16-bit and 8-bit registers.

To change the visibility of a function, and be able to call it from any file in
our program we use the `global` directive:

```nasm
global foo
```

When a function is called, the arguments are passed in the following order:
`rdi`, `rsi`, `rdx`, `r8`, and `r9`. Here's an example of a function that takes
a single argument and returns it, also known as an identity function:

```nasm
global identity
identity:
  mov rax, rdi
  ret
```

For the arithmetic operations addition, subtraction, and multiplication, we can
use the `add`, `sub`, and `imul` instructions. They take two operands, a source
operand (first operand), and a destination operand (second operand), performs
the arithmetic operation, and stores the result in the destination operand.
Here's an example of a function that takes two arguments, adds them together,
and returns the result:

```nasm
global sum
sum:
  mov rax, rdi
  add rax, rsi  ; rax += rsi
  ret
```

To call a function, we use the `call` instruction. For example, to call our
`sum` function with the arguments 3 and 5, we would do the following:

```nasm
mov rdi, 3  ; First argument in rdi
mov rsi, 5  ; Second argument in rsi
call sum
; The rax register now contains the value 8
```

Registers `rbp`, `rbx` and `r12` through `r15` are known as callee-saved
registers. This means that these registers must be preserved across function
calls. The rest of the registers are known as caller-saved and does not need to
be preserved.
