In C# applications randomness is generally achieved using the `System.Random` class.

This [article][system-random] is an excellent introduction to the subject.

When becoming familiar with a library class it is always worth studying the [documentation][system-random] for properties and methods and their overloads.

The coding exercise highlighted a number of issues:

- The random number generator does not require a [seed][random-seedless].
- `Random.Next()` can generate a range of integers
- `Random.Double()` can generate a double between 0 and 1
- Once you have your random number you can do what you like with it

There are 3 patterns for implementing the last point:

- You can get the random number and manipulate it. In the case of the coding exercise this would consist of calling `Random.NextDouble()` and multiplying by 100. This [piece][random-use-case-array] discusses making random selections from am array.
- You can inherit from `System.Random` and override the `Sampple()` method.
- You can encapsulate an instance of `System.Random` in a class of your own, for example, [to generate booleans][random-use-cases].

It is [recommended[random-thread-safety] that you instantiate `System.Random` as a static member. But, note that it is not thread safe.

You are advised not to use `System.Random` for crypto or security. See [this provider][crypto-provider]] and [this number generator][crypto-rng].

[system-random]: https://docs.microsoft.com/en-us/dotnet/api/system.random?view=netcore-3.1
[random-thread-safety]: https://docs.microsoft.com/en-us/dotnet/api/system.random?view=netcore-3.1#the-systemrandom-class-and-thread-safety
[random-use-cases]: https://docs.microsoft.com/en-us/dotnet/api/system.random?view=netcore-3.1#generate-random-boolean-values
[random-use-case-array]: https://docs.microsoft.com/en-us/dotnet/api/system.random?view=netcore-3.1#retrieve-an-element-from-an-array-or-collection-at-random
[crypto-provider]: https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=netcore-3.1
[crypto-rng]: https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.randomnumbergenerator?view=netcore-3.1
[random-seedless]: https://docs.microsoft.com/en-us/dotnet/api/system.random?view=netcore-3.1#instantiating-the-random-number-generator
