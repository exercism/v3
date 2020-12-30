To work on the exercises, you will need these pieces of software:

1. [`OPAM`, the OCaml Package manager](https://opam.ocaml.org/)

   See [Real World Ocaml, 2nd Ed.: Installation Instructions](https://dev.realworldocaml.org/install.html)
   for how to install and configure `OPAM` for your operating system.

2. The OCaml compiler

   See a list of available versions:

   ```bash
   opam switch
   ```

   Switch to that version. If, for example, the latest version is 4.08.0, you will run:

   ```bash
   opam switch 4.08.0
   ```

3. Install extended standard libraries and test libraries

   Some exercises use only the OCaml standard library, and some use the
   extended libraries by Jane Street called Base and Core\_kernel.

   The test library is called OUnit, and some exercises additionally use the
   QCheck library for property-based tests.

   ```bash
   opam install base core_kernel ounit qcheck
   ```

4. Install and use interactive shell

   A summary of [Setting up and using `utop`](https://dev.realworldocaml.org/install.html):

   ```bash
   opam install utop
   ```

   Place the following in the file `.ocamlinit` in your home directory should contain something like:

   ```ocaml
   #use "topfind";;
   #require "base";;
   open Base
   ```
