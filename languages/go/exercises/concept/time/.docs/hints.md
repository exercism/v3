## General

- [Use the methods found in the time package.][time]

## 1. Parse appointment date

- The `time` package has a `parse` method to [parse][time.parse] a `string` into a `Time`.

## 2. Check if an appointment has already passed

- `Time` types can be compared using the [before][before] and [after][after] methods.
- There is a [function][now] to retrieve the current date and time.

## 3. Check if appointment is in the afternoon

- Accessing the hour portion of a `Time` can de done with the [hour][hour] method.

## 4. Describe the time and date of the appointment

- Convert the given string to a `Time` then format the answer string accordingly, using the `time` methods to extract the needed constituents.

## 5. Return the anniversary date

- Use [time.parse][time.parse] to create a `Time` type of the anniversary date.

[time]: https://golang.org/pkg/time/#pkg-index
[time.parse]: https://golang.org/pkg/time/#Parse
[before]: https://golang.org/pkg/time/#Time.Before
[after]: https://golang.org/pkg/time/#Time.After
[now]: https://golang.org/pkg/time/#Now
[hour]: https://golang.org/pkg/time/#Time.Hour
