## Making Your First Node Module

To create a module that can be loaded with `Bob = require './bob'`, put this code in `bob.coffee`:

```coffeescript
class Bob

module.exports = Bob
```

You can find more information about modules in the [Node documentation](http://nodejs.org/api/modules.html#modules_module_exports).

## Visual Studio on Windows

If you have a paid version of Visual Studio, you can download a Visual Studio solution that is already set up to work with CoffeeScript and the other languages that Visual Studio supports.

![Solution Explorer](http://x.exercism.io/v3/tracks/coffeescript/docs/img/SolutionExplorer.png)

1. Download the [Exercism.io Visual Studio Template](https://github.com/rprouse/Exercism.VisualStudio) from GitHub by clicking the Download Zip button on the page.
2. Unzip the template into your exercises directory, for example `C:\src\exercises`
2. Install the [Exercism CLI](http://exercism.io/cli)
3. Open a command prompt to your exercise directory
4. Add your API key to exercism `exercism configure --key=YOUR_API_KEY`
5. Configure your source directory in exercism `exercism configure --dir=C:\src\exercises`
6. [Fetch your first exercise](http://exercism.io/languages/coffeescript) `exercism fetch coffeescript`
7. Open the Exercism solution in Visual Studio
8. Expand the Exercism.coffeescript project
9. Click on **Show All Files** in Solution Explorer (See below)
10. The exercise you just fetched will appear greyed out. Right click on the folder and **Include In Project**
11. Get coding...

![Add files](http://x.exercism.io/v3/tracks/coffeescript/docs/img/AddFiles.png)

To work with CoffeeScript in Visual Studio, you should install the [Web Essentials](http://vswebessentials.com/) extension. It will also make working with Javascript much easier.

You can run the unit tests from a node.js command line using the batch file in the project.

```
C:\Src\exercises\coffeescript>test example\bob_test.spec.coffee
.................

Finished in 0.02 seconds
17 tests, 17 assertions, 0 failures, 0 skipped
```

If you do not see any output from running the tests, you are likely not in a Node.js command prompt.

## Recommended Learning Resources

If you want to learn the basics about CoffeeScript you may want to try these resources:

* [Code School course on CoffeeScript](https://www.codeschool.com/courses/coffeescript)
* [CoffeeScript Website](http://coffeescript.org)
* [StackOverflow](http://stackoverflow.com/)
