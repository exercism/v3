C# types can be defined within the scope of a class or struct. The enclosing type provides a kind name space. Access to the type is through the enclosing type with dot syntax.

```csharp
class Outer
{
    interface IInner {}
    enum EInner {}
    class CInner {}
    struct SInner {}
}

var outer = new Outer();
var inner = new Outer.Cinner();
```

#### Access levels

Access levels can be applied to the inner types. For example if a class is private then it cannot be seen outside of the enclosing type's scope and of course instances cannot be seen or used outside the scope.

There is a pattern to prevent an instantiable type being created outside the enclosing type. You expose a public interface but keep the implementing class private.

The private members of the outer class are in scope for all the members of the inner class but only the non-private members of the inner class are in scope for the members of the outer class. In order to access the outer classes members the inner class has to be given an instance of the outer class.

This can be seen in the following example:

```csharp
class Outer
{
    public class InnerImpl
    {
        private Outer outer;

        public int Interesting = 42;

        public InnerImpl(Outer outer, int interesting)
        {
            this.outer = outer;
            Interesting = interesting;
        }

    }

    public InnerImpl Inner;
    public Outer()
    {
        Inner = new InnerImpl(this, 42);
    }
}

var outer = new Outer();
outer.Inner.Interesting
// => 42
var inner = new Outer.Inner(outer, 1729);
outer.Inner.Interesting
// => 1729

```

If you want to prevent the inner class being instantiated independently of the outer class then you can use an interface to expose the public methods of the inner class.

```csharp
class Outer
{
    interface IInner
    {
        void DoSomething();
    }

    private class InnerImpl : IInner
    {
        private Outer outer;
        public InnerImpl(Outer outer)
        {
            this.outer = outer;
        }
        public void DoSomething()
        {
            outer.DoSomethingElse();
        }
    }

    public IInner Inner;

    public Outer()
    {
        Inner = new InnerImpl(this);
    }
    private void DoSomethingElse()
    {
    }
}

var outer = new Outer();
outer.Inner.DoSomething();
```

~~var inner = new Outer.Inner(outer);~~

#### Note for Java developers

C#'s nested classes are a cross between Java's static nested classes and inner classes. Private members of the enclosing class are in scope for members of the nested class but there is no special syntax for instantiating them and linking them to an instance of the enclosing class. You have to use some variation of the pattern illustrated above.

C# does have static nested classes, but these are simply enclosed classes with static behavior.

#### Reference

This documentation of [nested types][nested-types] details the syntax.

[nested-types]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/nested-types
