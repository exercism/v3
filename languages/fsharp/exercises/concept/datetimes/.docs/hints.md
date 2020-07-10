## General

- To work with the `DateTime` class, the `System` namespace has to be _opened_.

## 1. Parse appointment date

- The `DateTime` class has a method to [parse][parsing] a `string` to a `DateTime`.

## 2. Check if an appointment has already passed

- `DateTime` objects can be compared using the default [comparison operators][operators].
- There is a [property][properties] to retrieve the current date and time.

## 3. Check if appointment is in the afternoon

- Accessing the time portion of a `DateTime` object can de done through one of its [properties][properties].

## 4. Describe the time and date of the appointment

- The tests are running as if running on a machine in the United States, which means that when converting a `DateTime` to a `string` will return dates and time in US format.
- There is a [built-in method][to-string] to convert a `DateTime` instance to a `string`. If you are using `sprintf`, instead of using the `%s` string placeholder, there is [another placeholder][object-placeholder] that will automatically call the aforementioned method.

## 5. Return the anniversary date

- Use one of the various `DateTime` [constructors][constructors] to create a new `DateTime` instance.
- You can use one of the current date time's [properties][properties] to get the current year.

[parse]: https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parse?view=netcore-3.1#System_DateTime_Parse_System_String_
[operators]: https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netcore-3.1#operators
[properties]: https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netcore-3.1#properties
[to-string]: https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring?view=netcore-3.1
[object-placeholder]: https://fsharpforfunandprofit.com/posts/printf/#formatting-for-dummies
[constructors]: https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netcore-3.1#constructors
