You are implementing an ORM (Object Relational Mapping) system over a database which has been provided by another team.

The database is capable of handling a single transaction at one time.

No logging or other error handling is required at this stage.

Note that internally the database transitions through a number of states: TransactionStarted, DataWritten, Invalid, Closed

### 1. Begin a transaction

Implement `Orm.Start()` to start a transaction on the database.

```csharp
var orm = new Orm(new Database());
orm.Start();
// => database has a state of State.TransactionStarted
```

### 2. Write some data to the database

Implement `Orm.Write()` to write some data to the database. If the write fails then clean up the database.

```csharp
var orm = new Orm(new Database());
orm.Start();
orm.Write("some data");
// => database has a state of State.DataWritten
orm.Write("bad write");
// => database has a state of State.Closed
```

### 3. Commit previously written data to the database

Implement `Orm.Commit()` to commit the data. If the commit fails then clean up the database.

```csharp
var orm = new Orm(new Database());
orm.Start();
orm.Write("some data")
orm.Commit();
// => database has a state of State.Closed
orm.Start();
orm.Write("bad commit")
orm.Commit();
// => database has a state of State.Closed
```

### 4. Ensure that the database is cleaned up correctly if the ORM has to close part way through a transaction.

Implement the `IDisposable` interface on the `Orm` class

```csharp
var db = new Database()
var orm = new Orm(db);
orm.Start();
orm.Write("some data")
orm.Dispose();
// => database has a state of State.Closed
```
