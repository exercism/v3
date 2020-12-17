## Prerequisites

The D language track requires that you have the following software
installed on your system:

* a D version 2 compiler
* (optional but recommended) DUB

### Prerequisite: D version 2 Compiler

On the D language's website [dlang.org](https://dlang.org) the most recent compiler
version of the reference compiler DMD (Digital Mars D) can be downloaded and installed:

#### Windows

* [Installer](http://downloads.dlang.org/releases/2.x/2.071.1/dmd-2.071.1.exe)
* or: [Archive](http://downloads.dlang.org/releases/2.x/2.071.1/dmd.2.071.1.windows.7z)
* using [chocolatey](https://chocolatey.org/packages/dmd): choco install dmd

#### Mac OS X

* .dmg [package](http://downloads.dlang.org/releases/2.x/2.071.0/dmd.2.071.0.dmg)
* or: [Archive](http://downloads.dlang.org/releases/2.x/2.071.0/dmd.2.071.0.osx.tar.xz)
* using [Homebrew](http://brew.sh/): brew install dmd

#### Linux / FreeBSD
To quickly install dmd within your user directory, run: *curl -fsS https://dlang.org/install.sh | bash -s dmd*

Packages for various distributions are provided:

* [ArchLinux](https://wiki.archlinux.org/index.php/D_(programming_language)
* [Debian/Ubuntu](http://d-apt.sourceforge.net/)
* [Fedora/CentOS](http://dlang.org/download.html#dmd)
* [Gentoo](https://wiki.gentoo.org/wiki/Dlang)
* [OpenSuse](http://dlang.org/download.html#dmd)

#### Other compilers
Besides the DMD reference compiler which uses its own backend, there are two other compilers that can
be fetched through the dlang.org download section:

* [GDC](http://gdcproject.org/downloads) which uses the GCC backend
* [LDC](https://github.com/ldc-developers/ldc#installation) based on the LLVM backend

GDC and LDC aren't always at the most recent DMD frontend's versions, but provide better optimization levels as well as
support for other platforms like e.g. ARM.

See the wiki for [more information](https://wiki.dlang.org/Compilers).

#### IDE support

There are also support for the D language in various IDEs e.g.
[VisualD](http://rainers.github.io/visuald/visuald/StartPage.html) for Visual Studio and
[D plugin](https://plugins.jetbrains.com/plugin/7727?pr=clion) for IntelliJ's CLion. See the wiki
for [more information](https://wiki.dlang.org/IDEs).

### Prerequisite: DUB

[DUB](https://github.com/dlang/dub) is a build manager for D.

We recommend it as a way to simplify the process of running the tests, but it is still possible to run without DUB if you cannot or do not want to install it.

#### Windows

The [DUB website's download page](https://code.dlang.org/download) has a Windows installer.

#### Mac OS X

* The [DUB website's download page](https://code.dlang.org/download) has OS X binaries.
* or using [Homebrew](http://brew.sh/): brew install dub

#### Linux

* The [DUB website's download page](https://code.dlang.org/download) has Linux binaries.
* Packages for various distributions are provided:
    * [ArchLinux](https://www.archlinux.org/packages/community/x86_64/dub/)
    * [Debian/Ubuntu](http://d-apt.sourceforge.net/)
    * [Gentoo](https://github.com/gentoo/dlang/tree/master/dev-util/dub)
    * [OpenSUSE](http://software.opensuse.org/download.html?project=devel%3Alanguages%3AD&package=dub)
