If a class implements the `IDisposable` interface then its `Dispose()` method must be called whenever it is no longer required.  This is typically done from a `catch` or `finally` clause.  `Dispose()` provides an opportunity for unmanaged resources such as operating system objects (which are not managed by the .NET runtime) to be released.

