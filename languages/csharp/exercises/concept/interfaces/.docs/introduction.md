An interface is a type containing members defining a group of related functionality. It distances the uses of a class from the implementation allowing multiple different implementations or support for some generic behavior such as formatting, comparison or conversion.

```csharp
public interface ILanguage
{
    string LanguageName { get; set; }
    string Speak();
}

public class ItalianTaveller : ILanguage, IClonable
{
    public string LanguageName { get; set; } =  "Italiano";

    public string Speak()
    {
        return "Ciao mondo";
    }

    public object Cloone()
    {
        ItalianTraveller it = new ItalianTraveller();
        it.Language = this.Language;
        return it;
    }
}
```

All operations defined by the interface must be implemented.

Interfaces can contain instance methods and properties amongst other members

The `IComparable<T>` interface can be implemented where a default generic sort order in collections is required.
