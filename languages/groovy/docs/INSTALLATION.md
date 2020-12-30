# Installation

## Installing Groovy on Unix (Mac OSX, Linux, Solaris or FreeBSD)

In addition to the exercism CLI and your favorite text editor, practicing with Exercism exercises in Groovy requires

* the **Java Development Kit** (JDK) — Groovy compiles to Java bytecodes; you need to install the JDK which includes both a Java Runtime *and* development tools (most notably, the Java compiler); and
* **Gradle** — a build tool specifically for JVM-based projects and supports Groovy.

The preferred method of installing both Java and Gradle is with [SDKman](http://sdkman.io/).

Detailed instructions can be found [here](https://sdkman.io/install). Briefly, open a shell and issue:

1. `curl -s "https://get.sdkman.io" | bash`
1. `source "~/.sdkman/bin/sdkman-init.sh"`
1. `sdk install java`
1. `sdk install gradle`

Side note: this method is also suitable for Cygwin and WSL on Windows

## Installing Java and Gradle on Windows

1. You should have JDK installed and `JAVA_HOME` environment variable set correctly. 
You can test it by issuing `javac -v` command. 
To install JDK follow [this instructions](https://docs.oracle.com/javase/10/install/installation-jdk-and-jre-microsoft-windows-platforms.htm)
1. You should have Gradle installed. 
You can test it by issuing `gradle -v` command. 
To install Gradle follow [this instructions](https://gradle.org/install/#manually)