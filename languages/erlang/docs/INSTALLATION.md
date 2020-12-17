If you have any trouble installing erlang please consider joining the 
[gitter support channel](https://gitter.im/exercism/xerlang)

### Homebrew for Mac OS X

Update your Homebrew to latest:

```bash
$ brew update
```

Install Erlang and Rebar3:

```bash
$ brew install erlang rebar@3
```

### On Linux

* Fedora 17+ and Fedora Rawhide: `sudo yum -y install erlang`
* Arch Linux: `sudo pacman -S erlang`
* Ubuntu/Debian: `sudo apt-get install erlang`

It may happen that the packages above are dated. At least for ubuntu 16.04
it should still be able to run the tests. If your package gets to old (older
than OTP 17.0) please consider a build from source or using [`kerl`](https://github.com/kerl/kerl)
or [`asdf-vm`](https://github.com/asdf-vm/asdf) and [`asdf-erlang`](https://github.com/asdf-vm/asdf-erlang)
instead (follow installation instructions in the corresponding repositories).

Also fetch the latest `rebar3` from rebar3.org and put it somewhere in
your `$PATH` and make it executable. (PRs that describe this better or
via package manager are welcome).

* Arch Linux: [install](https://wiki.archlinux.org/index.php/Arch_User_Repository#Installing_packages)
  the AUR package [`rebar3`](https://aur.archlinux.org/packages/rebar3).

### On Windows

Assuming [`choco`](https://chocolatey.org/) is available (maybe you
already installed `exercism` CLI using it).

```cmd
choco install erlang
choco install rebar3
```

### Installing from Source

Get [a recent Erlang OTP](http://www.erlang.org/download.html) and follow their
[build-instructions](https://github.com/erlang/otp/blob/maint/HOWTO/INSTALL.md).
