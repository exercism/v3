# Installation

We'll be using `PolyML` as our `Standard ML` implementation.

If you are using Windows you can download an installer from https://github.com/polyml/polyml/releases. General instructions for Linux/OS X/Free BSD can be found at https://github.com/polyml/polyml/blob/master/README.md.

If you want to install from source, the process is pretty easy:
```
$ git clone https://github.com/polyml/polyml.git
$ cd polyml
$ ./configure
```

If you want to install it in a custom location for instance `$HOME/.local`:
```
$ ./configure --prefix=$HOME/.local
```

Last step:
```
$ make compiler
$ make install
```

Check that it was installed correctly:
```
$ poly
```

if you see the interpreter, you are ready! if not, you'll probably have to add `$HOME/.local/bin` (or `<you prefix>/bin`) to your `$PATH`.
