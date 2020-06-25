using System;

// **** please do not modify the Database class ****
public class Database : IDisposable
{
    public enum State {TransactionStarted, DataWritten, Invalid, Closed}

    public State DbState { get; private set; } = State.Closed;
    private string lastData;
    public void BeginTransaction()
    {
        DbState = State.TransactionStarted;
    }

    public void Write(string data)
    {
        // this does something significant with the db transaction object
        lastData = data;
        if (data == "bad write")
        {
            DbState = State.Invalid;
            throw new InvalidOperationException();
        }

        DbState = State.DataWritten;
    }

    public void EndTransaction()
    {
        if (lastData == "bad commit")
        {
            DbState = State.Invalid;
            throw new InvalidOperationException();
        }

        DbState = State.Closed;
    }

    private void ReleaseUnmanagedResources()
    {
        // this will clean up the file system or something similar
    }

    public void Dispose()
    {
        DbState = State.Closed;
        ReleaseUnmanagedResources();
        GC.SuppressFinalize(this);
    }

    ~Database()
    {
        Dispose();
    }
}

public class Orm : IDisposable
{
    private Database database;

    public Orm(Database database)
    {
        this.database = database;
    }

    public void Begin()
    {
        database.BeginTransaction();
    }

    public void Write(string data)
    {
        try
        {
            database.Write(data);
        }
        catch (Exception e)
        {
            database.Dispose();
        }
    }

    public void Commit(string data)
    {
        try
        {
            database.EndTransaction();
        }
        catch (Exception e)
        {
            database.Dispose();
        }
    }

    public void Dispose()
    {
        if (database != null)
        {
            database.Dispose();
        }
    }
}
