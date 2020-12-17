Exercism will only download a test file. You will need to manually create the header and the source file associated with the exercise. You will need to generate an Xcode Project file with the test file, the header file (.h) and the source file (.m). Alternatively, you can use a test runner utility that's described below.

### Creating the project in Xcode

* Start Xcode and create a new project.
* Select OS X > Application and then Command Line Tool.
* Click Next and give it a project name using the ExerciseName is advised.
* Click Next and save the project in exercism's exercise directory.
* Now that the project is created click on Editor > Add Target.
* Select OS X > Test and select OS X Unit Testing Bundle.
* Name the target ExerciseName Tests.
* In the left pane (known as the navigator), open the Project navigator and expand the folder ExerciseName Tests and open the file ExerciseName_Tests.m.
* Replace its contents with the test file you got from exercism.
* In that file, replace all instances of "test_suite" with "ExerciseName_Tests".
* Navigate to the File Template library in the right pane (or use CTRL+OPTION+CMD+1) and drag the Cocoa Class template into the ExerciseName_Tests folder in the Project navigator.
* Name it ExerciseName and select ExerciseName Tests as its target.
* You will now have two new files in your ExerciseName_Tests directory: ExerciseName.h and ExerciseName.m.
* Click on your project in the Project navigator.
* Click on ExerciseName Tests in the targets list.
* Select Build Phases in the editor's navigation.
* Confirm that both your .m files are in the compile sources list.
* Use CMD+5 to navigate to the Test navigator.
* Right click the bundle named ExerciseName Tests and click Enable ExerciseName_Tests.
* Run the tests by clicking on the right pointing triangle that appears when hovering over the bundle in the Test navigator or use CMD+U.

Tests will be run through Xcode.

__Note:__ If you receive the error "No visible `@interface` for ExerciseName declares the selector ExerciseSelector," you followed the steps correctly, but haven't written anything in your header/implementation file(s). After you declare your method in the .h file and define it in the .m file, your tests should raise more helpful errors that will lead you towards completing the exercise. Read this [primer on Objective-C Classes](http://blog.teamtreehouse.com/beginners-guide-objective-c-classes-objects) for more in-depth information.

### A Test Runner

An alternative to manually generating the project file is to use a test runner utility written in ruby, [`objc`](https://rubygems.org/gems/objc/), that will create a project file for you with the test file, header file and source file.

```bash
$ gem install objc
```

Run the tests with:

```bash
$ objc -x ExerciseName
```

(Note the `-x`/`--xcodebuild` flag, which specifies using `xcodebuild` instead of `xctool`. The latter does not work with Xcode's latest releases.)

The objc utility uses the exercise name to find the test file, `ExerciseNameTest.m`, the header file, `ExerciseName.h` and source file `ExerciseName.m`. The files are inserted into a temporary Xcode Project and then `xcodebuild` is used to run the tests for the project.

While `objc` makes it so you never have to launch Xcode to complete these exercises, the error messages and feedback through the command-line are not as clear as through the Xcode user interface.
