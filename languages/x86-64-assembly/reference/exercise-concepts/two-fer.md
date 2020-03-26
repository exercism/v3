# Concepts of two-fer

[Example Implementation](https://github.com/exercism/x86-64-assembly/blob/master/exercises/two-fer/example.asm)

- [RIP-relative addressing](https://www.nasm.us/xdoc/2.14.02/html/nasmdoc6.html#section-6.2.1): `default rel` sets registerless instructions in 64-bit mode to RIP–relative.
- data section: the data section is used to store strings
- labels: labels are symbols for addresses. Used for loops, conditionals, functions entry points, and for addressing strings.
- [Declaring Initialized Data](https://www.nasm.us/xdoc/2.14.02/html/nasmdoc3.html#section-3.2.1): `db` is used for declaring strings
- [String Constants](https://www.nasm.us/xdoc/2.14.02/html/nasmdoc3.html#section-3.4.4): "One for" is a string constant
- [Defining Constants](https://www.nasm.us/xdoc/2.14.02/html/nasmdoc3.html#section-3.2.4): `equ` is used for defining string lengths
- text section: the text section is where you write your code
- [Exporting Symbols to Other Modules](https://www.nasm.us/xdoc/2.14.02/html/nasmdoc6.html#section-6.6): `global` is used to prevent linker errors when calling the `two_fer` function from C code
- functions: the `two_fer` label is a function entry point
- registers: different registers are used to store values across the `two_fer` function
- `mov` instruction: used to copy values between registers
- function arguments: `rdi` is used to pass 1st argument to functions. `rsi` is used to pass 2nd argument to functions.
- `lea` instruction: used to load the address of strings into a register
- brackets: used to get the memory contents at an address
- string instructions: `rep movsb` is used to copy strings
- conditional statements: used for checking if `name` is NULL
- `cmp` instruction: used for comparing values
- jump instructions: used to jump to different labels
- `byte` directive: used to read/write an 8-bit value from memory
- loops: used to copy the `name`
- `ret` instruction: used to return from a function
