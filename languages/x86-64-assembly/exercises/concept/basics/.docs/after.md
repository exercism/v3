In x86-64 assembly there are 16 general purpose 64-bit registers, `rax`, `rbx`,
`rcx`, `rdx`, `rsp`, `rbp`, `rsi`, `rdi`, and `r8` through `r15`. The registers
can also be accessed as 32-bit, 16-bit, or 8-bit. For example the low 32-bits
of `rax` can be accessed by `eax`, the low 16-bits by `ax`, and the low 8-bits
by `al`. The first four registers can even be accessed by the high 8-bits of a
16-bit register.

Illustration of how the bits are accessed for the `rax` register:

```
--------------------------------------------------
| 64-bit |                  rax                  |
|--------+---------------------------------------|
| 32-bit |                   |        eax        |
|--------+---------------------------------------|
| 16-bit |                             |    ax   |
|--------+-----------------------------+---------|
| 8-bit  |                             | ah | al |
--------------------------------------------------
```

Summary of how registers are identified for different access modes:

```
--------------------------------------------------------------------------
| 64-bit   |   rax |   rbx |   rcx |   rdx | rsp | rbp | rsi | rdi | rx  |
|----------+-------+-------+-------+-------+-----+-----+-----+-----+-----|
| 32-bit   |   eax |   ebx |   ecx |   edx | esp | ebp | esi | edi | rxd |
|----------+-------+-------+-------+-------+-----+-----+-----+-----+-----|
| 16-bit   |    ax |    bx |    cx |    dx |  sp |  bp |  si |  di | rxw |
|----------+-------+-------+-------+-------+-----+-----+-----+-----+-----|
| 8-bit    | ah/al | bh/bl | ch/cl | dh/dl | spl | bpl | sil | dil | rxb |
--------------------------------------------------------------------------
```

Writing to a 32-bit register also clears the upper bits, .e.g., `mov rax, 0` is
the same as `mov eax, 0`. This is not true for 16-bit and 8-bit registers.

Registers `rbp`, `rbx` and `r12` through `r15` are known as callee-saved
registers. This means that these registers must be preserved across function
calls. The rest of the registers are known as caller-saved and does not need to
be preserved.

Function arguments are passed in the following order: `rdi`, `rsi`, `rdx`,
`rcx`, `r8`, and `r9`.
