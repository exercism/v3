An interface is a type containing members defining a group of related functionality. It distances the uses of a class from the implementation allowing multiple different implementations or support for some generic behavior such as formatting, comparison or conversion.

The syntax of an interface is similar to that of a class or struct except that methods appear as the signature only and no body is provided.

```java
public interface Language
{
    string getLanguageName();
    string speak();
}

public class ItalianTaveller implements Language, ICloneable {

    public string getLanguageName() {
        return  "Italiano";
    }

    public string speak() {
        return "Ciao mondo";
    }

    public object Clone() {
        ItalianTaveller it = new ItalianTaveller();
        it.setLanguageName(this.getLanguageName());
        return it;
    }
}
```

All operations defined by the interface must be implemented by the implementing class.

Interfaces contains instance methods.

The `Comparable<T>` interface can be implemented where a default generic sort order in collections is required.
