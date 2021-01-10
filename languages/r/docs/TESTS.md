# Install `testthat`

Tests for this track make use of the `testthat` package.

To install the package, simply open up R and run:

`install.packages("testthat")`

You can check that the package is installed successfully by running:

`library(testthat)`

# Running Tests

Each problem will have a script named `test_<exercise-name>.R` (e.g. `test_hello-world.R`). 

To run the tests for an exercise, simply execute this R script.

In RStudio, this is as simple as opening the test script, and clicking "Source" at the top right of the source pane. This will load your solution code through the `source` command on the first line of the test script, and then execute the suite of `testthat` tests that follow.

Note however, that the `source()` command on the first line of the test script will only work if your solution script (`<exercise-name>.R`) is located in your working directory. If this is not the case, you'll need to either: 
- change your working directory eg. `setwd('exercism/r/hello-world/')`, or 
- update the first line of the test script e.g. `source('exercism/r/hello-world/hello-world.R')`

If you prefer, you can also run the tests from the command line with `Rscript path_to_exercise_directory/test_<exercise-name>.R` (e.g. `Rscript hello-world/test_hello-world.R`)

Another convenient way to run these tests is to make use of [testthat's `auto_test` function](https://www.rdocumentation.org/packages/testthat/topics/auto_test). Since exercism code and tests are located in the same folder, use this same path for both the `code_path` and `test_path` parameters. After running `auto_test()`, the tests for the exercise will be executed automatically each time you update/save your R code for the exercise. This can help facilitate rapid iteration while working on your solution.
