# Installation

## Installing Clojure CLI

Clojure provides command line tools for; Running an interactive REPL (Read-Eval-Print Loop), Running Clojure programs, Evaluating Clojure expressions.
We encourage you to use Clojure CLI with the exercises. To install the [Clojure CLI](https://clojure.org/guides/deps_and_cli);

### Windows
For installation instructions on Windows see: [Clojure CLI Installation](https://clojure.org/guides/getting_started#_installation_on_windows).

### Linux
For installation instructions on most platforms see: [Clojure CLI Installation](https://clojure.org/guides/getting_started#_installation_on_linux).

### Homebrew for Mac OS X
Update your Homebrew:

``` bash
$ brew update
```

Install Clojure CLI:
``` bash
$ brew install node clojure/tools/clojure
```

`Node` is required for node environment.

## Testing the installation

To test the correct installation, run `clojure --help` and it shows you the version if everything went well.

```bash
$ clojure --help
Version: 1.10.1.547
...
```
