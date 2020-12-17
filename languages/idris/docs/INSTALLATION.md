## Prerequisites

Before installing Idris, you will need to make sure you have all of the necessary libraries and tools. You will need:

 *  A fairly recent Haskell platform. Version 2013.2.0.0 should be sufficiently recent, though it is better to be completely up to date.
 *  The GNU Multiple Precision Arithmetic Library (GMP) is available from MacPorts/Homebrew and all major Linux distributions.

## Idris

The easiest way to install Idris, if you have all of the prerequisites, is to type:

```shell
cabal update; cabal install idris
```

This will install the latest version released on Hackage, along with any dependencies.
If, however, you would like the most up to date development version you can find it,
as well as build instructions, on GitHub at: https://github.com/idris-lang/Idris-dev.

## Necessary libraries

You also need to install some Idris libraries, which are requirements of our testsuite.

### `idris-testing`

This library provides the actual testing framework we use. It's installation is pretty simple.

```shell
pushd /tmp
git clone git@github.com:jfdm/idris-testing.git
cd idris-testing
make lib
make install # depending on your installation of idris this might need to be run as root
popd
```

If the tests will ever brake because of calling things that your installed version of `idris-testing` does not provide, you have to update it by repeating the steps above.
