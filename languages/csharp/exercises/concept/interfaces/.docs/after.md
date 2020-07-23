An interface defines a set of operations an instance of class or a struct can do.

```csharp
public interface ILanguage
{
	return Speak();
}
```

It's must to implement all defined operations by the implementer.

```csharp

public class ItalianTraveller : ILanguage
{
	public string Speak()
	{
		return "Ciao mondo";
	}
}

public class RussianTraveller : ILanguage
{
	public string Speak()
	{
		return "Привет мир";
	}
}
```

Interfaces can contain instance methods, properties, events, indexers, or any combination of those four member types

Members of an interface are public by default, however they can't contain instance fields, instance constructors.

Further, by design C# does not support multiple inheritance, but it facilitates multiple inheritance through interfaces.

Moreover, the concept of [polymorphism can be implented through interfaces][interface-polymorphism].

It is highly likely possible to have the same method name and signatures are in two different interfaces.
In order provide a distinct implementations of these methods, C# prvodes [explicit implementation of interfaces][explicit-implementation].

```csharp
public interface IFoo
{
	void X();
}

public interface IBar
{
	void X();
}

public class Census : IFoo, IBar
{
	void IFoo.X()
	{
		Console.Write("This is from Foo");
	}

	void IBar.X()
	{
		Console.Write("This is from Bar");
	}
}
```

[interface-polymorphism]: https://www.cs.utexas.edu/~mitra/csSummer2013/cs312/lectures/interfaces.html
[explicit-implementation]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/interfaces/explicit-interface-implementation
