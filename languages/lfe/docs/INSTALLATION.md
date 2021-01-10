First, make sure you have Erlang installed, following the instructions on the
[Erlang][1] installation page as appropriate.
[1]: /languages/erlang

### Homebrew for Mac OS X

Update your Homebrew to latest:

```bash
$ brew update
```

Install LFE:

```bash
$ brew install lfe
```

### Using a docker container
If you just want to quickly take a look at LFE without polluting your
system with new packages, you can just pull a docker container with
LFE preinstalled.

Let's fetch a Debian image with LFE:

```bash
$ docker pull lfex/debian  # it will take a while...
[...]
``` 

Now let's run the LFE REPL inside the container:

```bash
$ docker run -i -t lfex/debian lfe
Erlang/OTP 19 [ert[...]

lfe >
```

And let's write some LFE to test it:

```bash
lfe > (* 7 4)
28
lfe > (lfe_io:format "hello world~n" ())
hello world
ok
```

Nice! Use `(exit)` or Ctrl-C (C-c) twice to exit.


### Installing from Source
Install your system's "developer tools" or "essential build packages", `git`
and Erlang's `erl`.

For example, on Ubuntu 14.04:

```bash
$ sudo apt-get install build-essentials git erlang erlang-base-hipe
```.

Afterwards you can download and build LFE with the following:

```bash
$ git clone https://github.com/rvirding/lfe
$ cd lfe
$ make
$ make install
```

Once you have LFE compiled, you can start up the REPL (interactive session)
by executing the ``lfe`` binary:

```bash
$ ./bin/lfe
```

