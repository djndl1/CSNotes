The most central concept in any operating system is the _process_: an abstract of a running program. Processes are one of the oldest and most important abstractions that operating systems provide.

# Process

The process model is based on two independent concepts: _resource grouping_ and _execution_.

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

The OS maintains a _process table_ (_process control block_), with one entry per process. Each entry contains important information about the process' state, including its program counter, stack pointer, memory allocation, the status of its open files, its accounting and scheduling information and everything about the process that must be saved when context switch occurs.

Suppose a disk interrupt happens. (By hardware) The current program counter, program status word, and sometimes a few registers are pushed onto the current stack by the interrupt hardware. The computer jumps to the address specified in the interrupt vector. (By software) More registers are saved; new stack is setup; C interrupt service runs (to handle the disk); Scheduler decides which process to run next; a process is run.

modelling multiprogramming TODO

# Threads

The main reason for having threads is that in many applications, multiple activities are going on at once. Threads are lighter weight than processes, easier/faster to create and destroy than processes. Threads are useful on systems with multiple CPUs.

- **thread**: The abstraction of execution of a process is a _thread of execution_. The thread has a program counter, registers that hold its current working variables, a stack which contains the execution history with one frame for each procedure. Threads are the entities scheduled for execution on the CPU.

What threads add to the process model is to allow multiple executions to take place in the same process environment, to a large degree indepenedent of one another.

- **multithreading**: used to describe the situation of allowing multiple threads in the same process. Some CPUs have direct hardware support for multithreading and allow thread switches to happen on a nanosecond time scale.

A thread can be in:

- running

- blocked

- ready

- terminated

There are two main places to implement threads:

- user space: can be implemented on an OS that does not support threads. Each process needs its own private thread table to keep track of the threads in the process. A user space runtime is responsible for thread management. Makeing a local call into the runtime is much more efficient than calling a kernel call. User-level threads allow each process to have its own scheduling algorithm. Some major problems with user-level threads are: 1. blocking syscalls block the whole process, which defeats one of the major goals of having multiple threads. The whole process is blocked when a page fault occurs. 2. within a single process, there are no clock interrupts, making it impossible to schedule processes round-robin fashion. The scheduler might request a clock signal once a second to give it control, however, this is crude and messy to program.

- kernel: the hernel tacks all the threads in the system. In some systems, in order to save some overhead, when a thread is destroyed, it is marked as not runnable, but its kernel data structures are not affected. Later when a new thread must be created, an old thread is reactivated. The main disadvantage is that the cost of a syscall is substantial.

- hybrid: use kernel-level threads and then multiplex user-level onto some or all of them
