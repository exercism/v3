## Linux Hackers Installation (for advanced Linux use only)

We are still working out the best instructions here, but we know (and appreciate) that many Linux developers are very discerning when it comes to what they install and where things should go.

_If you do want to stick to a standard install, just return and follow [the normal setup](https://exercism.io/tracks/pharo-smalltalk/installation)._

The following instructions are very WIP and based on ArchLinux but may apply to your distro (and if they don't help us figure out improvements, and possibly upstream Pharo requests too). _Note: its not yet clear if these instructions work reliably, so we would appreciate any help to improve them._

- Install a Pharo VM into your preferred location. You might be able to use one in AUR or equivalent if pharo 7 is listed in there, OR alternatively download one from: [https://files.pharo.org/vm/pharo-spur64/linux/] (and choose a stable-timer vm)
- You next need to extract a Pharo.image file to place in your exercism directory. You can obtain this from: [https://files.pharo.org/image/70/] and pick the latest 64 bit zip file.
- Now try a simple command line test to verify if the VM and Image are working: `pharo Pharo.image eval "100 factorial"`. If all is well, you should get an answer.
- Now try the "load script" described in the OSX section. You may find you get an error mentioning `libgit2`, if this is the case you should also install it and try again. We have also had reports that installing `libcurl-gnutls` can help (but this is not yet proven)
- We have also had reports that for some libgit2 failures there is an internal loadModule change that we may backport from the development pharo. In this case you need to replace the method `unixModuleName` in Class `LGitLibrary` as follows:
  1. Launch a headfull Pharo by typing `pharo-ui Pharo.image`
  1. Press Ctrl-Space and type `unixModuleName`
  1. Select the `LGitLibrary >> unixModuleName` entry and press enter
  1. In the resulting browser, replace the method source for `unixModuleName` with the following (copy and paste it), press save (ctrl-s), and finally save and exit pharo.
```
LGitLibrary >> unixModuleName
	| pluginDir |
	pluginDir := Smalltalk vm binary parent.
	#('libgit2.so' 'libgit2.so.0')
		detect: [ :each | (pluginDir / each) exists ] 
		ifFound: [ :libName | ^ (pluginDir / libName) fullName ].

	self error: 'Module not found.' 
 ```

Now retry the aforementioned OSX exercism "load script". If everything is working properly, you should see loading progress bars flicker across the screen, and then a System Browser window will appear. If not, please visit the Pharo Discord (as outlined above).

Now you should return and follow the remaining [Getting Started Instructions](https://exercism.io/tracks/pharo-smalltalk/installation).
