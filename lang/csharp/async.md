a `Task` is control flow running in another dimension, in a more abstract level than threading.


[Task.Wait and Inlining][https://devblogs.microsoft.com/pfxteam/task-wait-and-inlining/]

# Error Handling

A try-catch-block does not work on `Task.Start()` since the control flow possibly moves out of the block long before the exception occurs. For an asynchronous task, any exception is aggregated into `AggregateException`. A continuation on fault can act as an exception handler, otherwise, waiting on a faulted task or trying to get the result will throw an `AggregationException`.

If an exception that occurs within a task goes entirely unobserved—that is, (1) it isn’t caught from within the task; (2) the completion of the task is never observed, via Wait(), Result, or accessing the Exception property, for example; and (c) the faulted ContinueWith() is never observed—then the exception is likely to go unhandled entirely, resulting in a process-wide unhandled exception.

# Cancellation

It is a bad idea to rudely abort a thread. Cancel any unfinished tasks rather than allowing them to run during application shutdown.

# Async/Await

TAP was created to address these key problems:

• There is a need to allow long-running activities to occur without blocking the UI thread.

• Creating a new thread (or Task) for non CPU-intensive work is relatively expensive when you consider that all the thread is doing is waiting for the activity to complete.

• When the activity completes (either by using a new thread or via a callback), it is frequently necessary to make a thread synchronization context switch back to the original caller that initiated the activity.

The `async` method is not the same as normal `Task`s. An async task is already started by the time a `Task` is returned. `await` only signifies the execution yieding and the result unwrapping.

An asynchronous task is not necessarily on a new thread, IO-bound async tasks are inheritantly asynchronous without a new thread since they are handled by the OS and the low-level hardware interrupt mechanism.

For an async task invoked on the UI thread, regardless of whether the await statements occur within an iteration or as separate entries, they will execute serially, one after the other and in the same order they were invoked from the calling thread. The underlying implementation is to string them together in the semantic equivalent of Task.ContinueWith() except that all of the code between the await operators will execute in the caller’s synchronization context.

## Async Streams (C# 8.0)

Asynchronous streams are supported to enable asynchronous iteration and the building of asynchronous collections and enumerable type methods using yield return.

TODO

## Task Scheduler and The Synchrnonization Context

`TaskScheduler` by default uses the thread tool to schedule tasks appropriately, determining how to safely and efficiently execute them, when to reuse them, dispose them or create them.

The synchronization context: a task executes and in turn the continuation tasks execute. The awaiting task consults the synchronization context so that a task can execute efficiently and safely.

The Synchronization context gets set automatically for types of applications where that is critical.


# `async void`: Non-Option

The guideline is to avoid `async void ` methods unless they are subscribers to an event handler.

Without a returned handle, an `async void` gives no way to know if it has been completely executed and to handle an exception. Any exception thrown on an `async void` method likely ends up on the UI `SynchronizationContext` effectively an unhandled exception.

# Async/Await with the Windows UI

One of the key advantages of the `async/await` pattern is that it leverages the synchronization context to ensure that continuation work—work that appears after the await statement—will always execute on the same synchronization task that invoked the await statement. This approach is of significant value because it eliminates the need to explicitly switch back to the UI thread to update a control.


# Event-Based Asynchronous Patterns

A single async method and a corresponding MethodNameCompleted event. Classes can optionally support cancellation, progress reporting and incremental results for each asynchronous method.
An asynchronous method may also support multiple pending calls (multiple concurrent invocations), allowing your code to call it any number of times before it completes other pending operations (_multiple-invocation_, which can be queried through an extra parameter`userState`).

# Asynchronous Programming Model

`IAsyncResult` and `BeginOperationName`/`EndOperationName` method pairs

1. `BeginOperationName` starts the async operation and returns an `IAsyncResult` which stores information about an asynchronous operation. It returns control to the calling thread immediately. A callback on completion may be supplied.

2. The `IAsyncResult` is supplied to `EndOperationName`. If the async operations hasn't been completed, the method call blocks.
