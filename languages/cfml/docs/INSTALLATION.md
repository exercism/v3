## Installation

The only thing you will need to run these exercises is [CommandBox CLI](https://commandbox.ortusbooks.com/getting-started-guide) and [Java 8](https://java.com/en/download/) installed.  CommandBox CLI is supported on Mac, Linux, and Windows, and the installation guide can be found here:

[https://commandbox.ortusbooks.com/setup/installation](https://commandbox.ortusbooks.com/setup/installation)

An example of getting CommandBox CLI setup if you're a Homebrew user on Mac would be:

```bash
brew install commandbox
```

Once you have the CLI installed, you can play withe REPL:

```bash
box repl
```

You can directly execute .cfm pages:

```bash
box exec myTest.cfm
```

And you can even start up a web server in any directory of your hard drive:

```bash
box server start
```

CommandBox has an interactive shell you can use that has enhanced tab completion and runs faster.  Enter it by just running the `box` binary with no arguments.

```bash
box
```
