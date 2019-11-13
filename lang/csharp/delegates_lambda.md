- delegate: a construct captures a reference to a method and pass it around like any other object and calls the captured method like any other method.

```csharp
public delegate TResult Func<in T1, in T2, out TResult>(in T1 arg1, in T2 arg2)
```

- `System.Func` family: delegates for referring to methods that return a value;

- `System.Action` family: delegates for referring to `void`-returning methods;

```csharp
public delegate void Action ();
public delegate void Action<in T>(T arg)
public delegate void Action<in T1, in T2>(
    in T1 arg1, in T2 arg2)
public delegate void Action<in T1, in T2, in T3>(
    T1 arg1, T2 arg2, T3 arg3)
public delegate void Action<in T1, in T2, in T3, in T4(
    T1 arg1, T2 arg2, T3 arg3, T4 arg4)
...
public delegate void Action<
    in T1, in T2, in T3, in T4, in T5, in T6, in T7, in T8,
    in T9, in T10, in T11, in T12, in T13, in T14, in T16(
        T1 arg1, T2 arg2, T3 arg3, T4 arg4,
        T5 arg5, T6 arg6, T7 arg7, T8 arg8,
        T9 arg9, T10 arg10, T11 arg11, T12 arg12,
        T13 arg13, T14 arg14, T15 arg15, T16 arg16)

public delegate TResult Func<out TResult>();
public delegate TResult Func<in T, out TResult>(T arg)
public delegate TResult Func<in T1, in T2, out TResult>(
    in T1 arg1, in T2 arg2)
public delegate TResult Func<in T1, in T2, in T3, out TResult>(
    T1 arg1, T2 arg2, T3 arg3)
public delegate TResult Func<in T1, in T2, in T3, in T4,
    out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4)
...
public delegate TResult Func<
    in T1, in T2, in T3, in T4, in T5, in T6, in T7, in T8,
    in T9, in T10, in T11, in T12, in T13, in T14, in T16,
    out TResult>(
        T1 arg1, T2 arg2, T3 arg3, T4 arg4,
        T5 arg5, T6 arg6, T7 arg7, T8 arg8,
        T9 arg9, T10 arg10, T11 arg11, T12 arg12,
        T13 arg13, T14 arg14, T15 arg15, T16 arg16)

public delegate bool Predicate<in T>(T obj)
```

A custom delegate looks like:

```csharp
public delegate bool Comparator(int first, int second);
```

```csharp
using System;

class DelegateSample
{
    public static void BubbleSort(int[] items, System.Func<int, int, bool> compare)
    {
        int i;
        int j;
        int temp;

        if (compare == null)
        {
            throw new ArgumentNullException(nameof(compare));
        }

        if (items == null)
        {
            return;
        }

        for (i = items.Length - 1; i >= 0; i--)
        {
            for (j = 1; j <= i; j++)
            {
                if (compare(items[j-1], items[j]))
                {
                    temp = items[j-1];
                    items[j-1] = items[j];
                    items[j] = temp;
                }
            }
        }
    }

    public static bool GreaterThan(int first, int second)
    {
        return first > second;
    }

    static void Main(string[] args)
    {
        int i;
        int[] items = new int[5];

        for (i = 0; i < items.Length; i++)
        {
            items[i] = int.Parse(System.Console.ReadLine());
        }

        BubbleSort(items, GreaterThan); // `GreaterThan` is a method group expression.

        Console.WriteLine();
        for (i = 0; i < items.Length; i++)
        {
            System.Console.WriteLine(items[i]);
        }
    }
}
```
