## Installation

The simplest way to install [Pharo](http://pharo.org) is to use a [zero conf](http://pharo.org/download#//*[@id="main"]/div/h2[3]) download from a terminal command line. 
<br/>_Note: If you are trying to upgrade an existing installation see the [upgrade instructions](https://github.com/exercism/pharo-smalltalk/blob/master/docs/UPGRADE.md)._

Windows users who do not have a UNIX style shell installed should skip to the [Windows Installation](#windows-installation-skip-for-osxlinux) instructions (below).<br/>
More advanced Linux users may want to refer to the [Linux Hackers Installation](https://github.com/exercism/pharo-smalltalk/blob/master/docs/LINUX.md).

### Linux/OSX Installation

For Linux/OSX (assuming you have [installed Exercism](https://exercism.io/getting-started) and have joined the [Pharo track](https://exercism.io/my/tracks/pharo-smalltalk)), navigate to your `/Exercism/pharo-smalltalk` directory and type:

```bash
curl https://get.pharo.org/64/ | bash
```

and launch Pharo with the exercism tools by pasting the following "load script":

```smalltalk
./pharo-ui Pharo.image eval "
Iceberg remoteTypeSelector: #httpsUrl.
Metacello new 
 baseline: 'Exercism'; 
 repository: 'github://exercism/pharo-smalltalk/releases/latest';
 load.
#ExercismManager asClass welcome.
"
```

If you have any TIMEOUT problems when loading the tools, you should exit Pharo and clear the git cache by typing:

```rm -rf pharo-local/iceberg/```

And then repeat the ```./pharo-ui ...``` example above.

If you continue to have problems please visit the Pharo Discord channel described on the [Pharo Community](http://pharo.org/community) page. 

If everything is working properly, you should see loading progress bars flicker across the screen, and then a System Browser window will appear.

**TIP:** When you come to exit Pharo, save your changes (choose the Pharo menu, and select "Save and Quit").
You can now start Pharo with Exercism already loaded, by simply typing:

```bash
./pharo-ui Pharo.image
```

### Windows Installation (skip for OSX/Linux)

We are still working out the best command line tool prerequisites for windows, however you can also install Pharo graphically using the [Windows Pharo Launcher](https://files.pharo.org/pharo-launcher/windows) (a handy tool for managing multiple Pharo images). This launcher is also [available](http://pharo.org/download) for OSX and Linux if you are GUI inclined.

1. Run the downloaded .msi file to install the Pharo Launcher.

2. Run Pharo launcher and on the left of the window is a list of various Pharo image templates that can be downloaded. For Exercism
exercises, Pharo 7.0 32-bit (stable) is recommended (64bit is currently in beta and has reported issues). It can be found under the Official Distributions heading in the Templates 
tree. Click to highlight an image template, then click the create image icon at the top middle (an orange cog shape), and give
it a name. 
3. Once the template is downloaded it will appear on the right in the Existing Images table. Click on it, and then click the launch button at the top right (a green _play_ arrow).

4. Once the Pharo image has started, open a Playground by choosing the _Tools_ menu, and selecting _Playground_, or use
`ctrl + o + w`. 

Finally, copy and paste the following snippet into the playground:

```smalltalk
Iceberg remoteTypeSelector: #httpsUrl.
Metacello new 
 baseline: 'Exercism'; 
 repository: 'github://exercism/pharo-smalltalk/releases/latest';
 load.
 
#ExercismManager asClass welcome.
```

Then evaluate the pasted code by highlighting all of it, right clicking then selecting _Do it_ from the menu (or use `ctrl + d`). 

If everything is working properly, you should see loading progress bars flicker across the screen, and then a System Browser window will appear.


## Getting Started (ALL)

When you launch Pharo, you will see a Welcome project, in a [System Browser](https://medium.com/@richardeng/pharo-quick-start-5bab70944ce2#3099) (_tip:_ if you ever lose this window you can open a new one from the Tools|System Browser menu).


![Pharo Welcome Screen](https://github.com/exercism/pharo-smalltalk/raw/master/docs/images/PharoWelcomeScreen.png)

The top, left hand panel of the System browser shows packages in your environment, and you will notice the install script has already configured
a package called `Exercism`, which contains a sub-project tag called `Welcome`. The second panel shows classes, which also has a class called `Welcome`. Underneath the classes panel there are 4 radio buttons, "Flat" (shows the classes in the package), "Hier." (shows a class hierarchy), "Inst. side" (shows instance methods), "Class side" (shows class methods/constructors).

If you click on the Comment tab, you can see the latest instructions for using Pharo Exercism. Read the comments and try running your first test.

For other file based languages you would normally jump to a terminal at this point, and use the exercism cli to fetch the
next exercise. While Pharo can work with files in a similar manner, the environment is actually tuned to work with live objects. Classes, methods and source code are interestingly all objects like everything else in Pharo. You will learn more about this over the course of this track.

## Loading Exercises

For Exercism, we have included a plugin that will let you retrieve and submit Pharo exercises from within the IDE.
All you need to do is right click on an exercise package (in this case, the Welcome package in the top left panel), and select the `Exercism | Fetch Exercise`
menu item. This will prompt you for an exercise name (e.g. hello-world), and when entered, it will retrieve it automatically for you.

Exercise names can be found on each exercise description on your [Pharo track](https://exercism.io/my/tracks/pharo) page (far right).
There is a *Download* instruction with a cli string that contains each exercise key. You can also use the copy button next to the cli instructions and paste the entire string into the fetch prompt (and we will extract the key automatically).

When you have entered a valid exercise, the plugin will retrieve the relevant code and display it in the System Browser, ready for
you to begin [coding with tests](tests.md).

When all your tests are passing, you can submit your solution using the same context menu for your exercise package (right click on the package tag and select `Exercism | Submit Exercise...`)

![Exercism Submit Menu](https://github.com/exercism/pharo-smalltalk/raw/master/docs/images/SubmitExercise.png)

Finally use the `Exercism | View Track Progress` menu to visit the exercism website and see what other exercises have been unlocked for you, as well as post questions about your submission for mentors to read.

<br/>
  
**Did you know:** *When you launch Pharo, you are actually restoring an execution image snapshot - similar to a VMWare Operating System image. This
is a powerful concept that allows you to suspend work mid operation, possibly even when debugging
something. When you next relaunch Pharo, you can then continue stepping through code in the restored debugger, or possibly continue a refactoring step.*

## How to Reset your CLI Token

If you ever need to reset your CLI token due to a security issue, you should visit your [Exercism Settings Page](https://exercism.io/my/settings) page
and use the reset token button. When you have done this, you need to clear your old token in Pharo Exercism by evaluating the following code in a Playground:

```smalltalk
ExercismHttpClient 
    reset; 
    promptForToken.
```

## How to Upgrade Pharo Exercism

From time to time we may need you to update the libraries in your Pharo Exercism image. Please follow [these instructions](https://github.com/exercism/pharo-smalltalk/blob/master/docs/UPGRADE.md)