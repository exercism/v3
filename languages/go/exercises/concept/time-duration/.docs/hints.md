## General

- [Use the methods found in the time package.][time]

## 1. Parse the Duration of the Class

- There is a [method][difference.parse] for parsing a `string` into a `Duration`.

## 2. Check if the class is over

- There is a [method][duration.parse] for returning the duration between two Times. Check whether the duration is greater than the class duration which is 1 hr.

## 3. Find out the time the class has exceeded

- First find out the time exceeded using the earlier method to find the duration. There is a [method][minutes] for returning the duration in minutes.

## 4. Display the time elapsed since the start of school 

- There is a [method][string] for returning the duration between two Times in string format.

## 5. Round of the extra time passed by the class in some given seconds

- There is a [method][truncate] for rounding off the time duration between two Times.


[time]: https://golang.org/pkg/time/#pkg-index
[duration.parse]: https://golang.org/pkg/time/#example_Time_Sub
[difference.parse]: https://golang.org/pkg/time/#example_ParseDuration
[minutes]: https://golang.org/pkg/time/#example_Duration_Minutes
[string]: https://golang.org/pkg/time/#example_Duration_String
[truncate]: https://golang.org/pkg/time/#example_Duration_Truncate
