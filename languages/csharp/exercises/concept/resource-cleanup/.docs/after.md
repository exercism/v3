The [`IDisposable`][idisposable] interface has two significant roles in C#:

- It indicates to users of the implementing class that they are responsible for letting the class know (by calling the [`Dispose()`][dispose] method) that it is no longer required so that it can release any unmanaged resources or reset its internal state as appropriate.
- In conjunction with the compiler and runtime the `IDisposable` interfacer supports the [`using` statement][using-statement] discussed in the `resource-lifetime` exercise.

It is possible but unlikely that `Dispose()` will be called through the interface in some sort of generic cleanup routine.

`IDisposable` is most commonly encountered with library classes that wrap operating resources such as [`System.IO.Stream`][stream] and [`System.IO.TextReader`][text-reader] which are covered in other exercises.

Most C# are predominantly concerned with managed resources. If the classes containing those resources implement `IDispoable` then the developer must ensure that `Dispose()` is called (by use of `catch` and `finally` clauses) when the class is no longer required. If a class has a member which implements `IDisposable` then they will most likely need to implement `IDisposable` in their own class and have `Dispose()` dispose of that member.

```csharp
public class TextHandler : IDispoable
{
    private TextReader reader = new TextReader(...);

    public void Dispose()
    {
        reader.Dispose();
    }
}
```

You will see references in the documentation to the [dispose pattern][dispose-pattern]. The dispose pattern includes calling `Dispose()` from a class's [finalizer][finalizer] and ensuring that disposal of resources in base classes is handled correctly. These resources are typically acquired through the [P/Inovke mechanism][native-interoperability] which gives access the operating system resources such as the file system, network, drawing resources etc.

```csharp
public class Database : IDisposable
{
    [DllImport("riskyStuff.so.6")]
    extern object AcquireAnUnmanagedResource();

    [DllImport("riskyStuff.so.6")]
    extern object UseAnUnmanagedResource(object resource);

    [DllImport("riskyStuff.so.6")]
    extern bool ReleaseTheUnmanagedResource(object resource);

    object resource;

    public void Open()
    {
        resource = AcquireAnUnmanagedResource();
    }


    public void DoStuff()
    {
        try
        {
            UseAnUnmanagedResource(resource);
        }
        finally
        {
            Dispose();
        }
    }

    public void Dispose()
    {
        ReleaseTheUnmanagedResource(object resource);
        GC.SuppressFinalize(this);
    }

    ~Database()
    {
        ReleaseTheUnmanagedResource(object resource);
    }
}

```

The [dispose pattern][dispose-pattern] documentation details how `IDisposable` should be used.
The [finalizer][finalizer] documentation discusses the role of object finaliizers.

[finalizers]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/destructors
[using-statement]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/using-statement
[idisposable]: https://docs.microsoft.com/en-us/dotnet/api/system.idisposable?view=netcore-3.1
[dispose]: https://docs.microsoft.com/en-us/dotnet/api/system.idisposable.dispose?view=netcore-3.1
[stream]: https://docs.microsoft.com/en-us/dotnet/api/system.io.stream?view=netcore-3.1
[text-reader]: https://docs.microsoft.com/en-us/dotnet/api/system.io.textreader?view=netcore-3.1
[native-interoperability]: https://docs.microsoft.com/en-us/dotnet/standard/native-interop/
[dispose-pattern]: https://docs.microsoft.com/en-us/dotnet/standard/garbage-collection/implementing-dispose
