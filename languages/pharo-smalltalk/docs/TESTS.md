At its most basic level, Exercism is all about the tests and 
[testing](https://github.com/exercism/docs/blob/master/language-tracks/exercises/anatomy/test-suites.md), as it drives your implementation forward and tells you when an exercise is complete.

## Immediate Feedback
Pharo has great support for working with tests, and incremental testing! You can run any test for an exercise by clicking on the orb next to a test case class or method.

![Browser Test Orbs](https://github.com/exercism/pharo-smalltalk/raw/master/docs/images/TestOrbs.png)


The test orbs are coloured according to the result of the last test run: 
- green for a pass 
- yellow for an assertion failure 
- red for a runtime error or exception

## Ordered Tests
The tests in Exercism exercises have been purposely numbered to give a defined order of execution (_e.g. test01_verifySomeProperty, test02_verifyAnotherProperty etc._). 

When working on an exercise it is recommended that you click on the orb for the first test, understand the failure and what is required to make it pass, and then add the code to your solution to make it work. In Pharo it is quite normal to make these changes in the debugger where you have access to both the code editor as well as a view of all the variables and parameters that can help you understand the problem. 

_NOTE: It is not a regular practice to label tests with an ordering prefix, and you should NOT do this when writing tests for your own project._

## It Can Run When Broken
Pharo is happy to run with code that is broken, and the debugger will simply re-open when an error is encountered (either a syntax error, bad value or even a missing class or method). This technique was a primary influence for test driven development, and you can get a taste of this approach when you try running the first test for any exercise. The debugger will immediately show that your solution class is not found (as you haven't written anything yet). Conveniently, there is a "Create" button that helps you add the missing class and continue running it until it either finishes, or another error is encountered.

 ![Creating a class in the debugger](https://github.com/exercism/pharo-smalltalk/raw/master/docs/images/DebuggerCreate.png)
 
 With your first test, you next encounter a second error as you haven't written any methods yet. Again, the "Create" button in the debugger lets you define one, and continue running. At this point, you can also click further down the stack trace and review the requirements of the failing test and modify your new method to make it pass. 

## Loving The Debugger
Later in your development cycle you might also find it useful to click further back in the stack and use the "Restart" button to resume program execution at an earlier point, so you can then step through your program to see what is going on. This is an important and useful way to understand why your program isn't working.  

As well as seeing variables in the debugger, you can also highlight any statement and click on inspect/print to see the result of evaluating it. This can be useful when testing results from a method, or for examining the internal state of an object. 

![Inspect a statement](https://github.com/exercism/pharo-smalltalk/raw/master/docs/images/DebuggerInspect.png)

Don't forget that you can also make changes to live code in the debugger, and saving a change will simply cause execution to restart in the method that was just saved. This allows you to experiment with changes and view their results while you are still "in the moment". 

In Summary, don't be afraid of using the debugger in Pharo, we view it as a valuable tool that aids in understanding and experimenting with a problem.

## Larger Test Groups and Automation
If you ever need to run larger groups of tests, you can also run them from the Package menu as well as use the Test Runner tool (accessed from the World menu or type `<meta> + OU`).
 
You can also run tests programmatically from the playground by print evaluating:
```
AllExercismTests suite run.
```

If you want to learn more about the internals of tests, you can read about [SUnit](https://en.wikipedia.org/wiki/SUnit) or browse the code in the TestCase hierarchy.
<br/><br/>

**Did you know:** *[TDD](https://en.wikipedia.org/wiki/Test-driven_development) was invented in Smalltalk via the introduction of the [SUnit](https://en.wikipedia.org/wiki/SUnit) test library*

