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

# IPC

Two main issues:

1. how to pass information to another process/thread

2. how processes/threads do not get in the way of others

To overcome race conditions, mutual exclusion is needed. The choice of appropriate primitive operations for achieving mutual exclusion is a major design issue in any operating system. 

- race conditions: When two or more processes are reading or writing some shared data and the final result depends on who runs precisely when.

- _critical region/section_: the part of the program where the shared resource is accessed. No two processes may be simultaneously inside their critical regions.

A good solution to mutual exclusion should meet the following criteria:

- No two processes may be simultaneously inside their critical regions.

- No assumptions may be made about the speeds or the number of CPUs. 

- No process running outside its critical region may block any process. 

- No process should have to wait forever to enter its critical region.

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
        while (turn == process && interested[other] == true);
}

void leave_region(int process)
{
        interested[process] = false;
}
```

- `tsl`, a hardware solution. `tsl` reads the contents of the memory word `lock` into register `rx` and then stores a nonzero value at the memory address `lock`. The memory bus is locked so that a second processor cannot acess the work in `lock`. An alternative solution is to use `xchg`: exchanges the contents of two locations atomically.

```asm
; a spin lock, fast if the wait is short
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

### IPC that blocks

- **semaphore**: using an integer variable to count the number of wakeups saved for future use. Two atomic actions that check the value, change it and possibly go to sleep/wake up are P (down) and V (up). This atomicity is essential to solving synchronization problems and avoiding race conditions. e.g. semaphore used on I/O devices and interrupt.

```c
#include <stdbool.h>

#define N 100

typedef int semaphore;

void down(semaphore*);

void up(semaphore*);

semaphore mutex = 1;    

// empty + full <= N always holds
semaphore empty = N;    // synchronization, ensure that the produce/consumer stops under certain conditions
semaphore full = 0;

void producer(void)
{
        int item;

        while (true) {
                item = produce_item();
                down(&empty);
                down(&mutex);
                insert_item(item);
                up(&mutex);
                up(&full);
        }
}

void consumer(void)
{
        int item;

        while (true) {
                down(&full);
                down(&mutex);
                item = remove_item();
                up(&mutex);
                up(&empty);
                consume_item(item);
        }
}
```

- **mutex**: a simplified version of the semaphore, in two states: locked, or unlocked. This may not need a kernel call.

```asm
mutex_lock:
  tsl rx, mutex
  cmp rx, #0
  jze ok
  call thread_yield
  jmp mutex_lock
ok: ret

mutex_unlock:
  mov mutex, #0
  ret
```

- **condition variable**: allows threads to block due to some condition not being met. Almost always mutexes and condition variables are used together. Condition variables, unlike semaphores, have no memory. The key point is that releasing the lock and going to sleep or obtaining the lock and waking up) must be an atomic operation so that the condition is still the same as when it was checked. 


- **futex** (fast userspace mutex, actually a mechanism to implement such a mutex): Efficient synchronization and locking is very important for performance.  A wait queue for processes is in the kernel. Suppose the lock variable is 1, a thread atomically decrements and tests the lock and inspects the result to see whether the lock was free. If it was not locked, the thread has claimed the lock, otherwise, a syscall puts the thread on the wait queue and the thread is blocked. When the locks released, if there's no blocked thread, the kernel is not involved. Here, the lock variable is a userspace data structure and can be modified in user mode. See `man 7/2 futex`. The Linxu futex is not a mutex, but a kernel syscall as a general building block for mutex and semaphore implementation.

```c
// 
       int futex(int *uaddr, int futex_op, int val,
                 const struct timespec *timeout,   /* or: uint32_t val2 */
                 int *uaddr2, int val3);
```

>  The  futex() system call provides a method for waiting until a certain condition becomes true. It is typically used as a blocking construct in the context of shared-memory  synchronization. When  using  futexes,  the  majority  of  the synchronization operations are performed in user space.  A user-space program employs the futex() system call only when it is likely  that  the program has to block for a longer time until the condition becomes true. 

[Basics of Futuxes](https://eli.thegreenplace.net/2018/basics-of-futexes/)

[Futex Overview](https://lwn.net/Articles/360699/)


- **monitor**: a higher-level synchronization primitive rather than the hard-to-use-and-easy-to-make-a-mistake mutexes and semaphores. A monitor is a colletion of procedure, variables and data structures that are all grouped together in a special kind of module or package. Only one task can be active in a monitor at any instant. Monitors are a programming-language construct. It is up to the compiler to implement mutual exclusion on monitor entries. The automatic mutual exclusion on monitor procedures guarantees that if the producer inside a monitor procedure discovers that the buffer is full, it will be able to complete the `wait` operation without having to worry about the possibility that the scheduler may switch to the consumer just before the `wait` completes.

```basic
monitor ProducerConsumer
  condition full, empty;
  integer count;

  procedure insert(item: integer);
  begin
        if count = N then wait(full);
        insert_item(item);
        count := count + 1;
        if count = 1 then signal(empty);
  end;

  function remove: integer;
  begin
        if count = 0 then wait(empty);
        remove = remove_item;
        count := count - 1;
        if count = N - 1 then signal(full)
  end;

  count := 0;
end monitor

procedure producer;
begin
    while true do
    begin
          item = produce_item;
          ProducerConsumer.insert(item)
    end
end;

procedure consumer;
begin
    while true do
    begin
          item = ProducerConsumer.remove
          consume_item(item);
    end
end;
```

```java
// Java version TODO
```

- **message passing**: low-level synchronization primitives don't work on distributed systems. Message passing is commonly used in parallel programming systems. Message loss, duplicates, authentication and performance are issues with message passing. Consider the producer-consumer problem without using shared memory. _If no message is available, the receiver can block until one arrives._ There is no race condition since they don't manipulate shared resource. Both the producer and the consumer operate on messages that they have received and suspend when there's no more messages to handle.

```c
#include <stdio.h>

#define N 100

struct message;
typedef struct message message;

void producer(void)
{
        int item;
        message m;

        while (true) {
                item = produce_item();
                receive(consumer, &m);
                build_message(&m, item);
                send(consumer, &m);
        }
}

void consumer(void)
{
        int item, i;
        message m;

        for (i = 0; i < N; i++)
                send(producer, &m);   // empty messages
        while (true) {
                receive(producer, &m);
                item = extract_item(&m);
                send(producer, &m);
                consume_item(item);
        }
}
```

Addressing can be done by assigning each process an identifier. A different way is to use a new data structure call a _mailbox_ to place a certain number of messages. When a process tries to send to a mailbox that is full, it is suspended until a message is removed from the mailbox.

- **barrier**: When a thread reaches the barrier, it is blocked until all processes have reached the barrier. This allows groups of threads to synchronize. e.g. multiple threads computing a matrix transformation iteratively.

- **avoiding lock - read-copy-update**: the key is to ensure that each reader either reads the old version of the data or the new one entirely. **RCU** decouples the removal and reclamation of the update.
