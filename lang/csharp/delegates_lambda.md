# Delegate

 delegate (`System.Delegate`, used by the compiler to derive a delegate type): a construct captures a reference to a method and pass it around like any other object and calls the captured method like any other method. (like a function pointer type in C).

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

        BubbleSort(items, GreaterThan); // `GreaterThan` is a method group expression, allowed in delegate creation expression 

        Console.WriteLine();
        for (i = 0; i < items.Length; i++)
        {
            System.Console.WriteLine(items[i]);
        }
    }
}
```

_multicasting_: A delegate can call more than one method when invoked, set up by `+=` and `+`, called in order, the reference parameters are passed sequentially to each of the delegates and any change by the first method is visible to the next one.. The return value or out parameters are the ones of the last method. Use `-=` and `-` to remove a method from the invocation list.

Delegate types do not exhibit structural equality. Two different delegate types cannot be converted to each other even if they seem to have the same parameter types and return type. the compiler transforms an anonymous function into a separately declared static method, which is then instantiated as a delegate and passed as a parameter. 

Lambda and anonymous methods are language constructs but not built into the CLR.

[Anonymous methods vs. lambda expressions](https://stackoverflow.com/questions/17709006/anonymous-methods-vs-lambda-expression)

> Beginning with C# 3, lambda expressions provide a more concise and expressive way to create an anonymous function. 

 Use a null forgiving `!` operator to assign to a multicast delegate if needed.

In case where one of the delegate throws an exception, enumerate all the delegates sequentially and catch any potential exception in the multicast delegate. Or if the delegates return values but anyway, the guideline is to avoid this scenario entirely by only returning `void`.

```csharp
List<Exception> exceptionCollection = new List<Exception>();
    foreach (Delegate handler in onTemperatureChange.GetInvocationList())
    {
        try
        {
            ((Action<float>) handler)(value);
        }
        catch (Exception exception)
        {
            exceptionCollection.Add(exception);
        }
    }
    if (exceptionCollection.Count > 0)
    {
        throw new AggregateException(
            "There were exceptions thrown by OnTemperatureChange Event subscribers.",
                exceptionCollection);
    }
```


## Delegate Internals

A delegate has a `System.Reflection.MethodInfo.MethodInfo` and a target specifying the object to invoke on (`null` for static methods).

Internally, multicast delegates maintain a list of `Delegate` objects.

# Events

Events are a special kind of multicast delegate that can only be invoked from within the class or struct where they are declared (the publisher class). It is a response to two common issues that might occur with ordinary multicast delegates:

1. using `+` instead of `+=`

2. calling the publishing delegate outside the publishing object.

`event` defines a new type of member.

```csharp
using System;

public class Thermostat
{
    public class TemperatureArgs : System.EventArgs
    {
        public TemperatureArgs(float newTemperature)
        {
            NewTemperature = newTemperature;
        }

        public float NewTemperature { get; set; }
    }

    
    */
    public event EventHandler<TemperatureArgs> OnTemperatureChange = delegate {};

    public float CurrentTemperature
    {
        get { return _CurrentTemperature; }
        set 
        {
            if (value != CurrentTemperature)
            {
                _CurrentTemperature = value;
                OnTemperatureChange?.Invoke(this, new TemperatureArgs(value));
            }
        }
    }
    private float _CurrentTemperature;
}
```

Only the containing is able to invoke the event, which provides the needed encapsulation. Assigning an empty delegate eliminate the need of null check. In case the event is reassgined a null, a check is still required. However, the `event` keyword restricts the assignment to occur only within the class.  

Events enable a class or object to notify other classes or objects when something of interest occurs. The class that sends (or raises) the event is called the _publisher_ and the classes that receive (or handle) the event are called _subscribers_.

- The publisher determines when an event is raised; the subscribers determine what action is taken in response to the event.

- An event can have multiple subscribers. A subscriber can handle multiple events from multiple publishers.

- When an event has multiple subscribers, the event handlers are invoked synchronously when an event is raised.

- Events that have no subscribers are never raised. (should be implemented so)

If subscribers can be added and removed from the delegate on different threads, it is wise (as noted earlier) to conditionally invoke the delegate or copy the delegate reference into a local variable (so that this variable does not change) before checking it for null (however it does not guarantee thread safety).

## Event Internals

`event` is a CIL construct.

 C# allows you to define custom `add` and `remove` blocks to provide a unique implementation for each aspect of the event encapsulation. 

```csharp
public event EventHandler<TemperatureArgs> OnTemperatureChange
  {
      add
      {
          _OnTemperatureChange =
              (TemperatureChangeHandler)
                  System.Delegate.Combine(value, _OnTemperatureChange);
      }
      remove
      {
          _OnTemperatureChange =x
              (TemperatureChangeHandler?)
                  System.Delegate.Remove(_OnTemperatureChange, value);
      }
  }
  protected EventHandler<TemperatureArgs>? _OnTemperatureChange;
```
