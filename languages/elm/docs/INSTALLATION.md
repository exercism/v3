The current version of elm is 0.19.1.
Installation instructions change as the language evolves.
For up-to-date installation instructions head to
[installation instructions of the elm guide][elm-install].

[elm-install]: https://guide.elm-lang.org/install/elm.html

## Additional Tools

**elm-test**

The tool used to run the tests from the command line is called elm-test.
Currently elm-test requires Node.js and npm, so if you haven't already
please [install first Node.js and npm][npm-insall].
Once you have npm installed, you can install elm-test with:

```shell
npm install --global elm-test@latest-0.19.1
```

More info available on the [npm home page of elm-test][elm-test].

[npm-install]: https://www.npmjs.com/get-npm
[elm-test]: https://www.npmjs.com/package/elm-test

**elm-format**

Most elm programmers use elm-format to format their elm code.
It is a zero configuration code formatting tool.
It may feel a bit frustrating at the beginning if you are not used
to automatic formatting tools, but once you get used to it,
you start wondering how you could code without it previously.
It can also be installed with npm:

```shell
npm install --global elm-format@latest-0.19.1
```

More info available on [elm-format home page on GitHub][elm-format].

[elm-format]: https://github.com/avh4/elm-format

**Text Editor**

Elm support is good for at least [IntelliJ][intellij],
and text editors compatible with the [elm language server][els]
such as VS Code, Sublime Text or Vim.

[els]: https://github.com/elm-tooling/elm-language-server
[intellij]: https://github.com/klazuka/intellij-elm

## Permission Errors with npm

You may be facing EACCESS permissions error with npm
when installing elm-test or elm-format.
Please follow instructions on npm website if that is the case.
[Resolving EACCES permissions errors when installing packages globally][eaccess].

[eaccess]: https://docs.npmjs.com/resolving-eacces-permissions-errors-when-installing-packages-globally
