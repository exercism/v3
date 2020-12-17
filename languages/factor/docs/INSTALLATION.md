# Installing Factor, and making Factor and Exercism play nice

[**Even if you already have Factor 0.98 installed, you still need to read the last section of this document for important information about testing Factor code in the Exercism environment.**](#play-nice)

---

## Installing Factor

To install Factor, you need a [nightly](#nightly) or [bleeding-edge](#autobuild-from-source) build.

***The last stable release of Factor ([0.97 released 2 Nov 2014](https://re-factor.blogspot.com/2014/11/factor-097-now-available.html)) is too old for use with Exercism.***

---
### Nightly

<http://factorcode.org> provides nightly binaries under the "Development release" section. These binaries are built from git, but only builds that pass are shown.

---

### (Auto)build from source

If you want the bleeding edge and even nightly is too old for you, then you will need a modern C++ compiler (GCC >=4.8 or Clang >=3.5), `make`, and `curl`.

With those simple requirements fulfilled, you can:

1. Download the `build` shell script: [**here** for Unix-likes / POSIX shells](https://raw.githubusercontent.com/factor/factor/master/build.sh) or [**here** for Windows](https://raw.githubusercontent.com/factor/factor/master/build.cmd). Put it in the directory under which Factor should be installed. A directory called `factor` containing the codebase will be created.

2. Run it like `./build.sh install`, or `.\build.cmd install` on Windows. <sub>This will clone Factor's `git` repository, build it, and download a Factor VM image from <http://factorcode.org>. This process may take between 2 and 20 minutes, scaling with your link and clock speeds.</sub>

3. You can now run the `factor` or `factor.exe` binary generated inside the `factor` subdirectory. Try `factor --help` for help. You can also access documentation from the command line, or by pressing <kbd>F1</kbd> in the GUI Listener, which will open the help browser.

4. In the future, use the `build.sh` or (`build.cmd` on Windows) inside the `factor` subdirectory to pull from git and rebuild in-place, and download a new VM image if needed.

The file structure may now look like this:

```
.
├── build.sh                   <- delete if you like
└── factor
    ├── basis
    ├── boot.unix-x86.64.image
    ├── build.cmd              <- use these from now on
    ├── build.sh               <-
    ├── core
    ├── extra
    ├── factor
    ├── Factor.app
    ├── factor.image
    ├── GNUmakefile
    ├── libfactor.a
    ├── libfactor-ffi-test.so
    ├── LICENSE.txt
    ├── misc
    ├── Nmakefile
    ├── README.md
    ├── unmaintained
    ├── vm
    └── work
```

You no longer need the top level `build` script, as in the future the downloaded one should be used.

---

### Manually build from source (not recommended)

Clone the [repository](https://github.com/factor/factor) and use `make` in that folder.

If `make` fails with an error about a non-present target, try `make factor` or, if you can read a Makefile, find the make target for your platform in `GNUmakefile`.

If *that* fails, open an issue on [factor/factor](https://github.com/factor/factor) containing the error -- the friendly developers will be happy to help.

---

#### Stable (not recommended)

**Note that Exercism and [the `exercism` vocabulary for Factor exercises on Exercism][ef] both require at least a [nightly build](#nightly)**.

Only use a stable release if you plan to skip automated testing or thousands of new features and bugfixes, and you do not plan to contribute to [Factor exercises on Exercism](https://github.com/exercism/factor).

To develop Factor exercises for Exercism, you need to be able to build and run the [`exercism` vocabulary][ef].

Download a Stable release binary for your platform from the "Stable release" section on <http://factorcode.org>, and run the installer.

---

## Making Exercism and Factor work together <a name="play-nice"> </a>


Due to limitations like name clashes and filename disagreements, it's rather difficult to use Factor's `tools.test` on Exercism exercises out of the box.

Instead, there is a [wrapper testing vocabulary][et] for the Factor workflow in an Exercism context. Users and Exercism maintainers alike can write and run tests in a familiar way, with much less overhead.

To test your Factor solutions in your `exercism/factor` folder, follow the directions in [the README](https://github.com/catb0t/exercism.factor#getting-started).

For information on using the `exercism.testing` vocabulary, refer to the [testing documentation](exercism.io/languages/factor#test), or to `exercism.testing`'s documentation by running `"exercism.testing" help` in Factor, after [installing `exercism.testing`](https://github.com/catb0t/exercism.factor/tree/master/README.md).

[ef]: https://github.com/catb0t/exercism.factor
[et]: https://github.com/catb0t/exercism.factor/tree/master/exercism/testing