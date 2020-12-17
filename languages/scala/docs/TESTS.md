## Running Tests

With `sbt` installed, the tests can be executed from the command line with:

```bash
$ sbt test
```

You can also use `sbt` to re-run your tests whenever the source files change:

```bash
$ sbt
>~ test
```

Note that all tests have been disabled except the first one for you to work on. 
To continue, just remove the `pending` keyword from the beginning of each test case.

Tests can also be run within the following IDEs

* [IntelliJ IDEA with Scala Plugin](https://www.jetbrains.com/idea/)
* [ScalaIDE](http://scala-ide.org/index.html)
* [NetBeans with Scala Plugin](https://netbeans.org/)

