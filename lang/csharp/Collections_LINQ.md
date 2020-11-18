.NET Standards and .NET Core don’t even include the nongeneric collections.


# Collection Initializers

A collection is a class that implements `IEnumerable<T>` at least, which requires `IEnumerator<T>` to be implemented. A more ideal requirement is to implement `ICollection<T>`. An enumerator is like a cursor or a bookmark in the sequence.

 the collection classes do not support `IEnumerator<T>` interfaces directly. Instead of the collection class maintaining the state, a different class—usually a nested class, so that it has access to the internals of the collection—will support the `IEnumerator<T>` interface and will keep the state of the iteration loop.

The actual implementation of `foreach` is conceptually like:

```csharp
System.Collections.Generic.Stack<int> stack = new System.Collections.Generic.Stack<int>();
int number;

using(
  System.Collections.Generic.Stack<int>.Enumerator
    enumerator = stack.GetEnumerator())
) {
    while (enumerator.MoveNext())
    {
        number = enumerator.Current;
        Console.WriteLine(number);
    }
}
```

`foreach` uses duck typing instead of requiring full `IEnumerable<T>` support. Neither the element count within a collection nor the items themselves can generally be modified during the execution of a foreach loop. 

An `System.InvalidOperationException` is generally thorwn upon accessing the enumerator if the collection is modified within a `foreach` loop, reporting that the collection was modified after the enumerator was instantiated.

# Stnadard Query Operators

Each method on `IEnumerable<T>` is a standard query operator (In Java stream); it provides querying capability over the collection on which it operates. In the context of LINQ and standard query operators, each lambda expression forms part of the overall query to be executed.  The query object represents the query, not the results. When you ask the query for the results, the whole query executes (perhaps even again) because the query object doesn’t know that the results will be the same as they were during a previous execution (if one existed). To avoid such repeated execution, you must cache the data that the executed query retrieves (using `ToCollectioName()`).

```csharp
      IEnumerable<Patent> patents = PatentData.Patents;
      IEnumerable<Patent> patentsOf1800 = patents.Where(
          patent => patent.YearOfPublication.StartsWith("18"));
      IEnumerable<string> items = patentsOf1800.Select(
          patent => patent.ToString());
          
          
    IEnumerable<string> fileList = Directory.EnumerateFiles(
    rootDirectory, searchPattern);
    var items = fileList.AsParallel().Select( // PLINQ
    file =>
    {
        FileInfo fileInfo = new FileInfo(file);
        return new
        {
            FileName = fileInfo.Name,
            Size = fileInfo.Length
        };
    });
```

- `Where(predicate)`: filtering, deferred execution

- `Select(mapping)`: project, transform

- `Count()`: the collection's `Count` property might be used if available, otherwise the whole collection will be enumerated.

- `Any()`: see if the collection is greater than zero

- `OrderBy()` (returns `IOrderedEnumerable<T>`), `ThenBy()` (works on `IOrderedEnumerable`) and there descending variants.  Repeated calls to OrderBy() would undo the work of the previous call such that the end result would sort by only the keySelector in the final OrderBy() call.

- `Join()`: inner join.

- `GroupBy()`: returns an `IEnumerable<IGrouping<key, Element>>` where each `IGrouping` has the same key.

- `GroupJoin()`: join and group by the outer sequence

- `SelectMany`

## `IQueryable<T>` and `Queryable`

LINQ provider TODO

# Anonymous Types

A pure C# feature, not a new kind of type in the runtime. When the compiler encounters the anonymous type syntax, it generates a CIL class with properties corresponding to the named values and data types in the anonymous type declaration. C# 7.0 tuple syntax has eclipsed anonymous types.

Use `var` sparingly to improve readability only.

# Query Expressions

With query expressions, many standard query operator expressions are transformed into more readable code, much like SQL. The C# query expression order also more closely matches the order in which operations are logically performed.

TODO

# Other Collections

To specify a nondefault sort order, call the overload of `List<T>.Sort()`, which takes `IComparer<T>` as an argument.
The `IComparer<T>` interface is typically used when there are many different possible ways of sorting a data type and none is obviously the best.
