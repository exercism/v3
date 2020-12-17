# Tests

The exercises all use the [Pester](https://github.com/pester/Pester) framework for mocking and testing.

All tests will be in a file named `[exercise].Tests.ps1` as is the standard for the Pester framework.

To run tests from PowerShell you can simply run:

``` PowerShell
Invoke-Pester
```

This will run all `.Tests.ps1` files it can find in the directory.

## Running tests in Visual Studio Code

You can run tests in visual studio code in a couple of ways:

1. Running the `.Tests.ps1` script from the Debug menu. Keyboard short cuts are: `F5` or `Shift + F5`
1. From within the editor by clicking on the "Run Tests" link above the `Describe` keyword in the test code itself
1. Configuring a Task to run the Pester framework