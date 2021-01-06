### Requirements

* A 64-bit x86 CPU
* A modern C compiler
* The Make build tool
* The Netwide Assembler (NASM)

Please note that this track only supports the calling convention of the System
V AMD64 ABI. Windows users will need to install Windows Subsystem for Linux
(WSL), or set up a Linux virtual machine.

### Linux

Install the required software using your system's package manager:

* Ubuntu, Debian: `$ sudo apt-get install gcc make nasm`
* Fedora: `$ sudo dnf install gcc make nasm`
* CentOS, RHEL: `$ sudo yum install gcc make nasm`
* Arch Linux: `$ sudo pacman -S gcc make nasm`
* OpenSUSE: `$ sudo zypper install gcc make nasm`

### macOS

Install the Xcode Command Line Tools:

```bash
$ xcode-select --install
```

Install NASM using [Homebrew](http://brew.sh/):

```bash
$ brew install nasm
```

### Windows

To install WSL, see https://docs.microsoft.com/en-us/windows/wsl/install-win10.
