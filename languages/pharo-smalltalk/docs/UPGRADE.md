## Upgrading

From time to time we may need you to update something.

### Pharo Image

If you need to update the libraries in your Pharo Exercism image, then it's best to ensure you have submitted any in-progress exercises, saved your image, and then backed up the Pharo.image and Pharo.changes files. Once you have a safe backup, evaluate (select, and press meta-g) all of the following code in a Playground:

 ```smalltalk
 
 './pharo-local/iceberg/exercism' asFileReference deleteAll.
 './pharo-local/package-cache' asFileReference deleteAll.
 
 IceRepository reset.
 
 Metacello new 
  baseline: 'Exercism'; 
  repository: 'github://exercism/pharo-smalltalk/releases/latest';
  onConflict: [ :ex | ex allow ]; 
  load.
  
 #ExercismManager asClass upgrade. 
 ```

You may be prompted about losing changes to the package "ExercismTools", and you should choose "Load" to ensure you have a compatible version of the tools. 

If you ever need to upgrade (or downgrade) to a specific version of Exercism, you can also modify the above script to specify a particular version number by changing the repository path as follows:
 
```smalltalk
 ... 
  repository: 'github://exercism/pharo-smalltalk:<version-tag>';
 ...
 ```

Where `<versison-tag>` could be something like: `v0.2.3` or `master`
 
Once you have loaded a specific version, you may also need to "re-fetch" any existing exercises that you want to continue working on by using the regular `Exercism | Fetch...` menu item.
 
In rare situations (and if you continue to have problems), you might need to get a fresh Pharo.image file (the easiest way is to re-install Pharo in a fresh directory by following the regular installation instructions at the top of this page).

### Pharo Exercises

Sometimes you may also find that an exercise has been updated to add new tests or to reflect new insights, after you have already solved it.
 
In these cases, you can choose to upgrade your copy of the exercise to the latest version - which means that you may need to adjust your solution to make the tests pass, and can then submit your new code for further review.

You can do this by using the `Exercism | View Track Progress` menu, which will open a web browser on your current track progress. In the `Test suite` tab, at the bottom of the page, there is an `Update exercise to latest version` button if a newer exercise version has been detected. 

If you click this button, and click the `Copy` button (in the Download your solution box), you can then paste this value into the `Exercism | Fetch new exercise` menu prompt.

_NOTE: as of version 0.2.8, the format of Exercise packages in Pharo Exercism was changed such that exercises appear in a top level package named: Exercise@<Name> (instead of a tag package called Exercism-<Name>). If you upgrade your image, and have old exercises that appear in this previous package naming format, you can still submit them, but if you also update the exercise test you will need to move your solution classes to the the new Exercise@<Name> package where the new test has been stored._

