a `Task` is control flow running in another dimension, in a more abstract level than threading.


[Task.Wait and Inlining][https://devblogs.microsoft.com/pfxteam/task-wait-and-inlining/]

# Error Handling

A try-catch-block does not work on `Task.Start()` since the control flow possibly moves out of the block long before the exception occurs. For an asynchronous task, any exception is aggregated into `AggregateException`. A continuation on fault can act as an exception handler, otherwise, waiting on a faulted task or trying to get the result will throw an `AggregationException`.

If an exception that occurs within a task goes entirely unobserved—that is, (1) it isn’t caught from within the task; (2) the completion of the task is never observed, via Wait(), Result, or accessing the Exception property, for example; and (c) the faulted ContinueWith() is never observed—then the exception is likely to go unhandled entirely, resulting in a process-wide unhandled exception.

# Cancellation

It is a bad idea to rudely abort a thread. Cancel any unfinished tasks rather than allowing them to run during application shutdown.
