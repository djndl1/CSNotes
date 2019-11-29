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

- user space: can be implemented on an OS that does not support threads. Each process needs its own private thread table to keep track of the threads in the process. A user space runtime is responsible for thread management. Makeing a local call into the runtime is much more efficient than calling a kernel call. User-level threads allow each process to have its own scheduling algorithm. Some major problems with user-level threads are: 1. blocking syscalls blocks the whole process, which defeats one of the major goals of having multiple threads. The whole process is blocked when a page fault occurs. 2. within a single process, there are no clock interrupts, making it impossible to schedule processes round-robin fashion. The scheduler might request a clock signal once a second to give it control, however, this is crude and messy to program.

- kernel: the kernel handles all the threads in the system. In some systems, in order to save some overhead, when a thread is destroyed, it is marked as not runnable, but its kernel data structures are not affected. Later when a new thread must be created, an old thread is reactivated. The main disadvantage is that the cost of a syscall is substantial.

- hybrid: use kernel-level threads and then multiplex user-level onto some or all of them.

Scheduler activations TODO http://polaris.imag.fr/vincent.danjean/papers/anderson.pdf

Pop-up threads TODO

Accessing global variables can cause problems from different threads. Many library procedures are not reentrant (they are not designed to have a second call made to any given procedure while a previous call has not yet finished). Signals are hard to handle in multhreading.

`errno` is thread-local.

# PIC

Two main issues:

1. how to pass information to another process/thread

2. how processes/threads do not get in the way of others

- race conditions: When two or more processes are reading or writing some shared data and the final result depends on who runs precisely when.

To overcome race conditions, mutual exclusion is needed. The choice of appropriate primitive operations for achieving mutual exclusion is a major design issue in any operating system. 

- _critical region/section_: the part of the program where the shared resource is accessed. No two processes may be simultaneously inside their critical regions. No assumptions may be made about the speeds or the number of CPUs. No process running outside its critical region may block any process. No process running outside its critical region may block any process. No process should have to wait forever to enter its critical region.

To achieve mutual exclusion

### Busy Waiting

- disabling interrupts: have each process disable all interrupts just after entering its critical region and reenable them just before leaving it. Disabling interupts is undesired and does not work for multiprocessor systems. Disabling interrupts is often a useful technique within the operating system itself but is not appropriate as a general mutual exclusion mechanism for user processes.

- strict alternation: not suitable for two processes at different execution rate. A process may be blocked by the other process even if the other is not in its critical region.

```c
int turn;
void critical_region();
void non_critical_region();

void process_0(void)
{
        while (turn != 0);
        critical_region();
        turn = 1;
        non_critical_region();
}

void process_1(void)
{
        while (turn != 0)
                ;
        critical_region();
        turn = 0;
        non_critical_region();
}
```

- Peterson's solution: lock variables and warning variables; A software solution. `enter_region` will ensure a process enters the region safely. If both processes try to enter the region, the first one that set `turn` will enter region while the second one busily waits.

```c
#include <stdbool.h>

#define N 2

int turn;
int interested[N];

void enter_region(int process)  // process is 0 or 1
{
        int other;

        other = 1 - process;
        interested[process] = true;
        turn = process;
        while (true == process && interested[other] == true);
}

void leave_region(int process)
{
        interested[process] = false;
}
```

- `tsl`, a hardware solution. `tsl` reads the contents of the memory word `lock` into register `rx` and then stores a nonzero value at the memory address `lock`. The memory bus is locked so that a second processor cannot acess the work in `lock`. An alternative solution is to use `xchg`: exchanges the contents of two locations atomically.

```asm
enter_region:
    tsl rx, lock
    cmp rx, #0
    jne enter_region
    ret
    
leave_region:
    mov lock, #0
    ret
```

```asm
enter_region:
    mov, rx, #1
    xchg rx, lock
    cmp rx, #0
    jne enter_region
    ret
    
leave_region:
    mov lock, #0
    ret
```

Busy waiting may encounter the _priority inversion problem_.
