An interface defines a set of operations an instance of class or a struct can do.

```csharp
public interface ILanguage
{
	return Speak();
}
```

All operations defined by the implementer must be implemented.

Interfaces can contain instance methods, properties, events, indexers, or any combination of those four member types

Members of an interface are public by default, however they can't contain instance fields, instance constructors.

Further, by design C# does not support multiple inheritance, but it facilitates multiple inheritance through interfaces.

Moreover, the concept of polymorphism can be implented through interfaces.

It is highly likely possible to have the same method name and signatures are in two different interfaces.
In order provide a distinct implementations of these methods, C# prvodes explicit implementation of interfaces.
