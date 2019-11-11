The most central concept in any operating system is the _process_: an abstract of a running program. Processes are one of the oldest and most important abstractions that operating systems provide.

# Process

- **sequential process** (process): an instance of an executing program, including the current values of the program counter, registers, and variables. Conceptually, each process has its own virtual CPU.

Normally, most processes are not affected by the underlying multiprogramming of the CPU or the relative speeds of different processes.

Four principal events cause processes to be created:

1. System initialization.

2. Execution of a process-creation syscall by a running process.

3. a user's interactive request to create a new process.

4. initialization of a batch job.

Technically, in all these cases, a new process is created by having an existing process execute a process creation syscall. In UNIX, the reason for two-step process creation is to allow the child to manipulate its file descriptors after the `fork` but before the `execve` in order to accomplish rediction of `stdin`, `stdout` and `stderr`.

A process terminates usually due to:

1. normal exit (voluntary); error exit (voluntary): A syscall is executed to tell the OS that it is finished.

3. fatal error (involuntary): an error is caused by the process, often due to a program bug.

4. killed by another process (involuntary)

A process may be in

1. running (actutally using the CPU at that instant)

2. ready (runnable; temporarily stopped to let another process run)

3. blocked (unable to run until some external event happens)

```
           +----------+
  +--------+ Running  +--------+
  |        +--------|-+        |
  |                 ^          |
  |                 +---+      |
  |                     |      |
  |                     |      |
  |                     |      |
  v                     |      v
+-|------+            +-|------++
| Blocked+----------->+  Ready  |
+--------+            +---------+
```

The process scheduler handles all these process state changes.
