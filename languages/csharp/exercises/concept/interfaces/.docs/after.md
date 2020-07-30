`interfaces` are the primary means of decoupling the users of a class from its implementation.

The syntax of an interface is similar to that of a class or struct except that methods and properties appear as the signature only and no body is provided.

```csharp
public interface ILanguage
{
	string Speak();
}

public interface IScriptConverter
{
	string ConvertCyrillicToLatin();
}
```

The implementing class must implement all operations defined by the interface.

```csharp

public class ItalianTraveller : ILanguage
{
	public string Speak()
	{
		return "Ciao mondo";
	}
}

public class RussianTraveller : ILanguage, IScriptConverter
{
	public string Speak()
	{
		return "Привет мир";
	}

    public string ConvertCyrillicToLatin(string cyrillic)
    {
        // do the conversion
    }
}

pubilc class DocumentTranslator : IScriptConverter
{
    public string Translate(string russion)
    {
        // do the translation
    }

    public string ConvertCyrillicToLatin(string cyrillic)
    {
        // do the conversion
    }
}
```

Interfaces typically, either:

- expose a subset of functionality for some specific purpose (such as [`IComparable`][icomparable]) or,
- expose the public API of a class so that multiple implementations can co-exist.

They are widely used to support testing as they allow for easy [mocking][so-mocking-interfaces].

No multiple inheritance.

Explicit interfaces

Default implementation

Serializable, Cloneable etc.

Interfaces can contain instance methods, properties, events, indexers, or any combination of those four member types.

Members of an interface are public by default, interfaces can't contain instance fields or instance constructors.

Interfaces can contain nested types.  The interfaces act as namespaces in the same way that classes and structs do and the behaviour and syntax is identical.

Further, by design C# does not support multiple inheritance, but it facilitates a kind of multiple inheritance through interfaces.

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
[so-mocking-interfaces]: https://stackoverflow.com/a/9226437/96167
[icomparable]: https://docs.microsoft.com/en-us/dotnet/api/system.icomparable-1?view=netcore-3.1
