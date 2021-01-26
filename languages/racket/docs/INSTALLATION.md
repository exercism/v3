Please reference Racket's [Getting Started](http://docs.racket-lang.org/getting-started/) page for instructions on downloading and installing Racket.

Alternative installation options and further notes for specific OSes follow.

## macOS

Install via Homebrew:

```
brew update
brew install minimal-racket
```
The `minimal-racket` package will probably work for solving the problems, but it does not include the graphical interface IDE, DrRacket. If you want that, you have to install the full racket distribution from cask:

```
brew tap caskroom/cask
brew install racket
```

### macOS official installer
Note that on macOS, the official installer puts the files under `/Applications`, and as a result the `racket` and `raco` executables are not accessible on command-line terminals. You have to add the `bin` subdirectory below the Racket installation location (e.g., `/Applications/Racket v6.8/bin` for version 6.8) to the `$PATH`.  


## Linux

Debian:
```
sudo apt-get install racket
```

Arch:
```
pacman -S racket
```
