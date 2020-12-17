# Installing Emacs

Since Emacs Lisp (elisp) is the extension language for Emacs, you'll need to
install [GNU Emacs](http://www.gnu.org/software/emacs/) first.

It is recommended to get the latest Emacs, currently 24.5-1, but 24.4 or higher
should be considered the target implementation for the exercises.

### On OS X
The easiest way is to visit [Emacs For Mac OS X](http://emacsformacosx.com/) and download the dmg, which will
install GNU Emacs as a packaged OS X app.

Alternatives exist, such as Aquamacs (not recommended) or installing via
Homebrew, if you prefer.

### On Linux
On some distros, Emacs is already installed, since it's part of GNU.

#### Ubuntu
Prior to Ubuntu 15.04 "vivid vervet", the highest available Emacs version was
24.3, so you'll need to use a [PPA](https://launchpad.net/ubuntu/+ppas?name_filter=emacs) or [build from source](http://linuxg.net/how-to-install-emacs-24-4-on-ubuntu-14-10-ubuntu-14-04-and-derivative-systems/) if you're on 14.10 or
earlier.

Otherwise, if you're running vivid, it should be as simple as

```
sudo apt-get install emacs
```

#### Arch
Arch currently ships with the latest Emacs 24.5-1, so run:

```
sudo pacman -S emacs
```

and you should be set.

#### Other distros
Check with your distro to see what version is current, and use your package
manager or build from source as appropriate.

### Windows
So you've decided to install Emacs on Windows.

![](http://www.zeldauniverse.net/wp-content/uploads/2012/01/83-Image-2.jpg)

I've never done it, but the prevailing wisdom is that you just need to visit the
[FTP archive](http://ftp.wayne.edu/gnu/emacs/windows/), grab the correct binary (=emacs-24.x-bin-xxx.zip=), unzip it and
launch. YMMV, please let us know in the issues or [on Gitter](https://gitter.im/exercism/support) if this section
needs some love.
