The most central concept in any operating system is the _process_: an abstract of a running program that turns a physical CPU into multiple virtual CPU (only orignally without multithreading). Processes are one of the oldest and most important abstractions that operating systems provide.

A process is no longer a *sequential process* in the literal sense, rather, it represents the entire execution state of a program, an activity with a program, input, output and a state, including the execution flows, the data, security context etc. A process is the unit of modern *multiprogramming* (multipe programs，多道程序设计).

> all activities are divided over a number of sequential processes. These sequential processes are placed at various hierarchical levels, in each of which one or more independent abstractions have been implemented. 

# Process

The process model is based on two independent concepts: **resource grouping** and **execution**.

- **sequential process** (process): an instance of an executing program, including the current values of the program counter, registers, and variables. Conceptually, each process has its own virtual CPU.

Normally, most processes are not affected by the underlying multiprogramming of the CPU or the relative speeds of different processes.

Four principal events cause processes to be created:

1. System initialization.

2. Execution of a process-creation syscall by a running process.

3. a user's interactive request to create a new process.

4. initialization of a batch job.

Technically, in all these cases, a new process is created by having an existing process execute a 
process creation syscall. In UNIX, the reason for two-step process creation is to allow the child 
to manipulate its file descriptors after the `fork` but before the `execve` in order to 
accomplish redirection of `stdin`, `stdout` and `stderr`.

A process terminates usually due to:

1. normal exit (voluntary); error exit (voluntary): A syscall is executed to tell the OS that it is finished.

2. fatal error (involuntary): an error is caused by the process, often due to a program bug. e.g. Illegal instruction, terminating signals

3. killed by another process (involuntary), on Unix that is also caused by a signal but on Windows this is not the case for the unconditional `TerminateProcess`.

A process may be in

1. running (actutally using the CPU at that instant)

2. ready (runnable; temporarily stopped to let another process run)

3. blocked (unable to run until some external event/condition happens)

```
             ┌─────────────┐
             │             │
     ┌───────┤   Running   │
     │       │             ◄─────┐
     │       └─────────────┘     │
     │                           │
     │                           │
┌────▼───────┐              ┌────▼──────┐
│            │              │           │
│   Blocked  ├─────────────►│   Ready   │
│            │              │           │
└────────────┘              └───────────┘
```

The process scheduler handles all these process state changes.

The OS maintains a _process table_ (_process control block_), with one entry per process. Each entry contains important information about the process' state, including its program counter, stack pointer, memory allocation, the status of its open files, its accounting and scheduling information and everything about the process that must be saved when context switch occurs.

context switch: Suppose a disk interrupt occurs. (By hardware) The current program counter, program status word, and sometimes a few registers are pushed onto the current stack by the interrupt hardware. The computer jumps to the address specified in the interrupt vector. (By software) More registers are saved; new stack is setup; C interrupt service runs (to handle the disk); Scheduler decides which process to run next; a process is run.

Suppose there are $n$ processes, each spending $p$ fraction of its time waiting for I/O, then the probability of all processes are waiting for I/O is $p^{n}$, the CPU must have at least one process running on it otherwise, then the CPU utilization is 

$$
1 - p^{n}
$$

With high I/O accesses, more processes are favored in order to better utilize the CPU.

A better modeling should be constructed using _queuing theory_.

# Threads

The main reason for having threads is that in many applications:
-  multiple activities are going on at once and threading makes them easier to manage
-  Threads are lighter weight than processes, easier/faster to create and destroy than processes
- Threads are useful especially on systems with multiple CPUs for performance reason
-  with I/O to release execution from waiting on I/O

**thread**: The abstraction of execution of a process is a _thread of execution_. The thread has a program counter, registers that hold its current working variables, a stack which contains the execution history with one frame for each procedure. Threads are the entities scheduled for execution on the CPU.

Today a thread may not be a single execution flow, but a bookkeeping record, a container for many execution flows to make use of, not just in a userspace thread pool, sometimes even in the kernel.

What threads add to the process model is to allow multiple executions to take place in the same process environment, to a large degree independent of one another. 
Threading separates the concepts of _resource grouping_ and _execution_. Processes share physical hardware resources while threads shared an address space and other resources.

- **multithreading**: used to describe the situation of allowing multiple threads in the same process. Some CPUs have direct hardware support for multithreading and allow thread switches to happen on a nanosecond time scale.

Multithreading adds complications into the programming model: the behavior and semantics of Unix `fork` when the parent process is multithreaded; thread synchronization;

A thread may be in: running, blocked, ready, terminated.

There are two main places to implement threads:

- *Userspace*: can be implemented _on an OS that does not support threads_. Each process needs its own private thread table to keep track of the threads in the process. A user space runtime is responsible for thread management. Making a local call into the runtime is _much more efficient_ than calling a kernel call. User-level threads allow each process to _have its own scheduling algorithm_. Some major problems with user-level threads are: 
  - blocking syscalls blocks the whole process, which defeats one of the major goals of having multiple threads. Either the system calls have to be nonblocking or it is possible to determine in advance whether it will block. 
  - The whole process is blocked when a page fault occurs. 
  - Cooperative scheduling: within a single process, there are no clock interrupts, making it impossible to schedule threads  preemptively. The scheduler might request a clock signal once a second to give it control, however, this is crude and messy to program.
  - threads are often used in situations where they are frequently blocked, to overcome the negative effects of blocking calls to provide responsive results and high throughput. Kernel threads may immediately switch on blocking calls without much cost since the kernel has already taken control. For CPU-bound tasks, userspace threads are not of much value.

- *Kernel*: the kernel handles all the threads in the system. In some systems, thread structures in the kernel are recycled and reused. The main disadvantage is that the cost of a syscall is substantial.

- *hybrid*: use kernel-level threads and then multiplex user-level onto some or all of them.

- [Scheduler activations](http://polaris.imag.fr/vincent.danjean/papers/anderson.pdf): 

- **pop-up thread**: start up a new thread in a distributed system when an incoming message is received.

## Multithreading Versus Event-Driven

I/O operations are inherently asynchronous and concurrent, as they are primarily handled by peripheral devices rather than the CPU. There is no need to force a CPU execution flow concept (thread) on I/O operations.

In a multi-threaded program, each run of a thread constitutes a task. The threading runtime manages the switching between these tasks, taking responsibility for maintaining and managing their states. A thread acts as a vessel that records a task’s execution state; the runtime uses this saved state to let the CPU resume execution at the correct point and yield when appropriate.

In contrast, an event-driven program uses a single thread with asynchronous or non-blocking I/O. Here, the program maintains its own task states, saving a task's I/O state before moving on to another operation. The thread runs an event loop that continually checks all saved states; when an async I/O operation completes, the loop picks up that specific task and resumes its execution. This process is cooperative—tasks explicitly yield and switch when they cannot proceed due to logic requirements.

Notably, the main thread in an event-driven program effectively simulates the threading runtime's mechanism through this cooperative approach: it saves a task’s execution state whenever it is blocked, and only resumes the task when its required condition is met.

Ultimately, this demonstrates that multitasking does not necessarily require multi-threading.

```c
// event-driven web server 
inputHandles = { }
outputHandles = {  }

dataDict<Handle, Data> = {  }

while (true) {
    readyInputHandles, readyOutputHandles = check_available_handles(inputHandles, outputHandles, NO_WAIT);
    for (readyHandle in readyInputHandles) {
        if (readyHandle == listening_socket_handle) {
            // new task is started
            newSocketHandle = listening_socket_handle.accept(); // a new connection from a client
            inputHandles.add(new_socket_handle)
            // yield to wait for request data
        } else {
            // continue a previous task to receive request data
            request = readyHandle.receive(); // new data from the client
            response = handle_request(request);
            dataDict[readHandle] = response;
            
            outputHandles.add(readyHandle); // for sending responses
            // yield to wait for output 
        }
    }
    
    for (readyHandle in readyOutputHandles) {
      // continue a previous task to send response data
      saved_response = dataDict[readyHandle]; // previous saved response
      n = readyHandle.send(saved_response); // sent n bytes
      
      if (n < saved_response.length) { // not yet finished
          dataDict[readyHandle] = saved_response[n:]; // save the unfinished part
          // yield for further output
      } else {
          // request finished, destroying tasks
          dataDict.remove(readyHandle);
          outputHandles.remove(readyHandle);
          // the task is now terminated
    }
}
```

In this phased execution model, request handling is divided into three or more sequential stages. The thread processes pending tasks by iterating through these stages from first to last, checking at each one which requests are ready to proceed and handling them accordingly.

At the end of every stage, the request handling flow cooperatively yields. For a task to advance to the next stage, its associated I/O handles must be actively monitored so that the runtime can determine when it is ready to continue. As a result, pending tasks are distributed across all stages, and the program must systematically inspect and process them in order, always starting from the first stage.

This design naturally frames each stage as beginning with I/O calls that may be ready and ending with calls that may potentially block, thereby establishing clear and consistent boundaries for yielding and resuming execution.

## Issues When Making Existing Code Multithreaded

### Global Variables

Accessing global variables can cause problems from different threads. Thread-local storage is one way to handle this. Either create a storage area for each thread and pass it to every procedure of the thread or implement it with library procedure calls.

### Reentrancy

Many library procedures are not reentrant: they are not designed to have a second call made to any given procedure while a previous call has not yet finished (e.g. stateful function). Either rewrite the library or add some kind of mutual exclusion.

### POSIX Signals

Signals are hard to handle in multithreading as they are not designed for this. Some signals are logically thread-specific while others have semantics unrelated to threading.

# IPC & Synchronization

Main issues:

1. how to pass information to another process/thread

2. how processes/threads do not interfere with others

3. Proper sequencing when dependencies are present among processes/threads.

For inter-process communication (IPC) and synchronization, certain data must be shared among processes or threads. However, operations performed within the sections of code that access this shared data can lead to race conditions.

- _critical region/section_: the part of the program where the shared resource is accessed. 

- _race conditions_: the system's substantive behavior is dependent on the sequence or timing of uncontrollable events, leading to unexpected or inconsistent results. For a software system A computer program has multiple code paths that are executing at the same time. If the multiple code paths take a different amount of time than expected, they can finish in a different order than expected, which can cause software bugs due to unanticipated behavior.

Race conditions arise because the sequence of execution—including context switches—is not controlled by the processes or threads themselves. However, what can be controlled is the access to the shared resource. By forcing a specific ordering of that access, we can avoid race conditions. The core challenge is that processes or threads interfere with one another when they are inside a critical section.

To eliminate this conflict, we can enforce exclusive ownership of the shared resource for a short period—a mechanism known as _mutual exclusion_. The choice of appropriate primitive operations for achieving mutual exclusion remains a major design issue in any operating system.

A good solution to mutual exclusion should meet the following criteria:

- No two processes may be simultaneously inside their critical regions.

- No assumptions may be made about the speeds or the number of CPUs. 

- No process running outside its critical region may block any process. 

- No process should have to wait forever to enter its critical region.

To achieve mutual exclusion

### Mutual Exclusin with Busy Waiting

- _disabling interrupts_ to achive mutual exclusion: Context switch relies on clock interrupts. Have each process disable all interrupts just after entering its critical region and reenable them just before leaving it so that no context switch can take place. Disabling interupts is undesirable and does not work for multiprocessor systems. 
Disabling interrupts is often a useful technique within the operating system itself but is not appropriate as a general mutual exclusion mechanism for user processes.

- _atomic instructions_: A pure software solution using a "locked" variable is not enough due to the operation of test and enter not being atomic and thus suffers from the same race condition. With a little help (atomic instructions) from the hardware, this can be done:

```asm
enter_region:
	tsl register, lock   // read lock and set lock in a single instruction by locking the memory bus
	cmp register, #0     // the second one that executes tsl always sees 1
	jne enter_region     // spin and test
    ret

leave_region:
	move lock, #0
	ret

// or
enter_region:
	move register, #1
	xchg register, lock  // exchange = read lock into register and set lock to 1, the same as tsl
	cmp register, #0
	jne enter_region
	ret

leave_region:
	move lock, #0
	ret
```

- _strict alternation_: not suitable for two processes at different execution rate. There is no read-test-write race condition because there is no test-and-set operation, one process tests to enter the critical region and the other decides whether it can enter. This works only if the two processes work in strict alteration: if one process is too fast, enters and leaves the critical region and then the noncritical region while the other is still in the noncritical region, the first one will be blocked at the while loop because the latest `turn` value is set by itself and the only value set by the other process can make it proceed. In this solution, a process cannot enter the critical section in succession.

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
        while (turn != 1);
        critical_region();
        turn = 0;
        non_critical_region();
}
```

- Peterson's solution: lock variables and warning variables; A software solution. `enter_region` will ensure a process enters the region safely. If both processes try to enter the region, the last one that sets `turn` will enter region while the second one busily waits. No read-test-write here. A simple assignment is atomic.
`interested` indicates contention. A process can only enter the critical region when there is no contention and it's fast enough to enter without losing its `turn`.
There is no test-and-set, it's set-and-acquire-or-fail. The problem with nonatomic test-and-set is setting based on stale value while with  this solution, we enter by testing.

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

In essence, what these solutions do is this: when a process wants to enter its
critical region, it checks to see if the entry is allowed. If it is not, the
process just sits in a tight loop waiting until it is.

Busy waiting may encounter the _priority inversion problem_ (the lower-priority
process in its critical section may not be scheduled to run while the higher-priority process is busy waiting).

### Blocking Operations

/The Producer-Consumer Problem/ (Bounded Buffer Problem): Two processes share a common, fixed-size
  buffer. One of them, the producer, puts information into the buffer, and the
  other one, the consumer, takes it out. The Producer process must not produce
  an item if the shared buffer is full. The Producer process must not produce an
  item if the shared buffer is full. At any given instance, only one process
  should be able to access the shared buffer and make changes to it.

A naive solution to the producer-consumer problem uses sleep and wakeup calls: the producer goes to sleep when the buffer is full, and the consumer goes to sleep when the buffer is empty. Likewise, the producer is woken up when the buffer is no longer full, and the consumer when it is no longer empty.

However, this approach suffers from a critical race condition. The operations of checking the item count and then deciding to sleep (or wake) are not atomic—they can be interrupted by a context switch.

Consider this classic deadlock scenario: the consumer checks the buffer, finds it empty, and decides to sleep. But just before it actually executes the sleep operation, a context switch occurs. The producer runs, fills the buffer, and sends a wakeup signal to the consumer. At that moment, however, the consumer is still awake (since it hasn't gone to sleep yet), so the wakeup is effectively lost. The producer, now seeing the buffer is full, goes to sleep. Finally, the consumer resumes and completes its sleep operation. The result is that both processes remain asleep forever—the producer waits for space, and the consumer waits for data.

The underlying problem is that this test-and-act sequence can be interrupted, causing a process to take action based on stale or incorrect information, thereby breaking the intended state transition. A simple fix is to notify the consumer that it should stay awake using a wakeup waiting bit, The more consumers, the more bits to use.

/The Readers and Writers Problem/: It is acceptable to have multiple processes reading the database at the same time, but if one process is updating (writing)
the database, no other processes may have access to the database, not even readers.

#### Semaphore

A mechanism designed proposed to solve the producer-consumer problem by making 
the operations indivisible (atomic).

- **semaphore**: using an integer variable to count the number of wakeups saved
  for future use. Two atomic actions that check the value, change it and
  possibly go to sleep/wake up are P (proberen, down, wait) and V (verhogen, up, signal). This
  atomicity is essential to solving synchronization problems and avoiding race
  conditions.
  
- /P/, /down/, /wait/: Decrements the value of semaphore variable by 1. 
  If the new value of the semaphore variable is negative, the process executing
  wait is blocked. Otherwise, the process continues execution,
  having used a unit of the resource.
  ```c
  if (--semaphore < 0) sleep();
  ```
  
- /V/, /up/, /signal/: Increments the value of semaphore variable by 1. After
  the increment, if the pre-increment value was negative (meaning there are
  processes waiting for  a resource), it transfers a blocked process from the
  semaphore's waiting queue to the ready queue.
  ```c
  if (semaphore++ < 0) wake_up();
  ```

```c
#include <stdbool.h>

#define N 100

typedef int semaphore;

void down(semaphore*);

void up(semaphore*);

semaphore mutex = 1; // protect the buffer, use of a binary semaphore for mutual exclusion

// empty + full <= N always holds
semaphore empty = N;    // synchronization, ensure that the produce/consumer stops under certain conditions
semaphore full = 0;
// the reason why a full semaphore is required is that the up operation does not automatically block the producer

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

A single semaphore is not enough: `empty` and `full` semaphores are required for the producer/consumers to sleep and wake up (/synchronization/)
so that certain event sequences do or do not occur (proper state transition).
A dedicated binary semaphore `mutex` here ensures /mutual exclusion/ and prevents chaos. 
Mutual exclusion and synchronization are unrelated and the `mutex` is not supposed to protect the two synchronization semaphores (no `down(&full)` after `down(&mutex)` ), otherwise a producer cannot fill the buffer if the `mutex` is held by a sleeping consumer. `up(&empty/full)` can be put inside the critical region however but it 
causes extra delay if the notified consumer runs immediately and gets blocked by the mutex.

The exact implementation may use a few atomic instructions and possibly locks the memory bus. 

 e.g. semaphore used on I/O devices and interrupt: a process waiting on an empty
 semaphore after starting an I/O operation until the interrupt comes in and
 signals that semaphore to allow the process to handle the interrupt.

For the reader/writer problem:

```c
typedef int semaphore;
semaphore mutex = 1;
semaphore db = 1;
int rc = 0;    // reader count

void reader(void)
{
	while (TRUE) {
		// mutex protects rc
		down(&mutex);
		rc = rc + 1;
		if (rc == 1) down(&db); // first reader enters, acquires the db
		up(&mutex);
		// db protects the database
	   read_database();
	   
	   down(&mutex);
	   rc = rc - 1;
	   if (rc == 0) up(&db); // last reader leaves, releases the db
	   up(&mutex);

	   use_data_read();
   }
}
// the potential problem is that too many readers may hog the database and no writers can enter
void writer(void)
{
	while (TRUE) {
		prepare_data( );
   		down(&db);
		write_database( );
		up(&db);
	}
}
```

- **mutex**: a special version of binary semaphore, in two states: locked, or unlocked. This may not need a kernel call. Also, a mutex has a owner, a specific thread/process. A mutex may be implemented by a binary semaphore but not required.

```asm
mutex_lock:
  tsl rx, mutex
  cmp rx, #0
  jze ok
  call thread_yield    // yield instead of testing in a loop and no more dead locks if there is no context switch (user threads, priority inversion)
  jmp mutex_lock       // if a lock is held, it means someone else needs the CPU to finish its job in the critical section.
ok: ret

mutex_unlock:
  mov mutex, #0
  ret
```

- **condition variable**: allows threads to block due to some condition not being met. Almost always mutexes and condition variables are used together. Condition variables, unlike semaphores, have no memory since it is not a counter unlike a semaphore. 
The key point is that releasing the lock and going to sleep or obtaining the lock and waking up must be an atomic operation so that the condition is still the same as when it was checked .The mutex ensures that no one can change the condition before being blocked or after waking up to check the condition again. A consumer about to sleep will not be siganled since it holds the lock, and will only be signaled after being blocked since the lock is now released and possibly taken by the producer.

#### Futex

- **futex** (fast userspace mutex, actually a mechanism to implement such a mutex): Efficient synchronization and locking is very important for performance.

In most cases, a lock is not contended and so a simple atomic operation is enough for locking. In case of contension, the thread may busily wait if it finds the atomic variable is already in a locked state, which is totally in userspace but not efficient. It may sleep and so requires the kernel to step in.

A wait queue for processes is in the kernel. Suppose the lock variable is 1, a thread atomically decrements and tests the lock and inspects the result to see whether the lock was free. 
If it was not locked, the thread has claimed the lock, 
otherwise, a syscall puts the thread on the wait queue and the thread is blocked. 
When the locks released, if there's no blocked thread, the kernel is not involved. 
Here, the lock variable is a userspace data structure and can be modified in user mode. 
See `man 7/2 futex`. The Linux futex is not a mutex, but a kernel syscall as a general building block for mutex and semaphore implementation.

The linux futex is a 32-bit integer with atomic operations and associated with a kernel queue that records blocked threads. It lets userspace code ask the kernel to suspend the thread until a certain condition is satisfied and lets other userspace code to signal the condition and wake up waiting processes. The `futex` syscall does not assign any meaning to the value.

```c
// 
       int futex(int *uaddr, int futex_op, int val,
                 const struct timespec *timeout,   /* or: uint32_t val2 */
                 int *uaddr2, int val3);
```

[Basics of Futuxes With a mutex implementation](https://eli.thegreenplace.net/2018/basics-of-futexes/)

[Futex Overview](https://lwn.net/Articles/360699/)

#### Monitor

- **monitor**: a higher-level synchronization primitive rather than the hard-to-use-and-easy-to-make-a-mistake mutexes and semaphores. A monitor is a colletion of procedure, variables and data structures that are all grouped together in a special kind of module or package. Only one task can be active in a monitor at any instant. 
Monitors are a programming-language construct. It is up to the compiler to implement mutual exclusion on monitor entries. 

1. Only one proess can be in the monitor at a time. A process acquires the mutex upon enter. The acquisition and release of the mutex is automatic on enter and exit.

2. Mutual exclusion is not enough. A monitor adds `signal` and `wait` condvar procedures for synchronization.
  - `wait` causes the calling process to block and allows another process blocking at the entry to enter now, typical condvar behavior.
  - `signal`: wakes up a `wait`ing process before exiting the monitor.
  - the key is the automatic mutual exclusion conbined with `wait`: as long as a process is running inside a monitor, it has the mutex. `wait ` and `signal` 
    are not simple `sleep` and `wakeup`: they have implicit acquire-release operations on the mutex and no activity from the producer before a consumer calling `wait`
    actually goes to sleep because there IS a mutex held by the consumer.

```basic
monitor ProducerConsumer
  condition full, empty;
  integer count;

  procedure insert(item: integer);
  begin
        if count = N then wait(full);        # if there is only one producer and one consumer and both are well-behaved, if is fine
        insert_item(item);                   # use while otherwise
		count := count + 1;                  # being woken up does not mean the condition is met: conditional variable is not the condition itself, just a channel to communicate the condition
        if count = 1 then signal(empty);     # 1. another thread might come in and steal the condition
  end;                                       # 2. for multiple waiting threads, only first one actually has the condition
                                             # 3. spurious wakeup: the waiting thread just wakes up for no reason even if the condition is not met due to OS problems or                                             # some badly-behaved signaling thread that has not fulfill the condition.
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

#### Message Passing

**message passing**: low-level synchronization primitives based on `TSL` or `XCHG` don't work on distributed systems. Message passing is commonly used in parallel programming systems. 

Main issues with message passing:

- Message loss: acknowledge

- duplicates: lost acknowledge causes retransmission and duplicates, solved with sequence numbers.

- authentication

- performance are issues with message passing

Consider the producer-consumer problem without using shared memory. _If no message is available, the receiver can block until one arrives._ The producer also blocks on full. 
There is no race condition since they don't manipulate shared resource. Both the producer and the consumer operate on messages that they have received and suspend when there's no more messages to handle.

- **mailbox**: a place to buffer a certain number of messages. Both `send` and `receive` uses a mailbox as its address. A producere process blocks on a full mailbox and a consumer blocks on an empty mailbox.
 - a mailbox is similar to a MQ but not required to be FIFO, typically for a single receiver

- **rendezvous**: no buffer is required: both `send` and `receive` block waiting and run in strict lockstep.

```c
/*
 * tow mailboxes are used, one for producer and another for consumer, with a size of N.
 * this is not a simple push-pull: the producer only sends a message if it receives an empty-slot message.
 *                                 the consumer sends back an empty reply to notify the producer it has received one and the producer can send another.
 *                                 This is more like a request-response: the consumer request by sending an empty message, the producer responds by sending a full message
 *  There will be no message flooding or starvation: both parties work in a loose lockstep.
 */
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

e.g. the **Message Passing Interface** standard protocol.

#### Barrier

- **barrier**: When a process reaches the barrier, it is blocked until all processes have reached the barrier. This allows groups of threads to synchronize. 
e.g. multiple threads computing a matrix transformation *iteratively*, that is, no thread should proceed to the next iteration before others.

- **memory barrier/fence**:  enforce an order to guarantee that all memory operations (to read or write memory) started before the barrier instruction will also finish before the memory operations issued after the barrier. An enforced sequence point.

```c
// THREAD 1:
while (turn != 1) { } /* loop */
printf ("%d\n", x);


// THREAD 2:, turn = 1 might be executed before x = 100, thread 1 might not print x = 100
x = 100;
turn = 1;
```

#### Priority Inversion

A lower-priority thread holds a mutex while a preempting higher-priority thread needs the mutex to move on, causing a deadlock.
Solution: if the priority is not high enough, make it higher

- (naive) disabling all interrupts

- /priority ceiling/: a priority is assigned to the mutex and the holding thread has this priority. As long as contending threads 
  have no higher priority, no priority inversion occurs.
  
- /priority inheritance/: the holding thread temporarily inherit the priority of the contending higher-priority threads

- /random boosting/: give random holding threads a high priority

#### Lock-free

- **read-copy-update**: properly design an algorithm to ensure that each reader either reads the old version of the data or the new one entirely. 
  **RCU** decouples the removal and reclamation phases of the update.  Of course atomic operations are still required.
  RCU needs to carefully determines the maximum time (grace period, read-side critical section) a reader may hold a reference to the old version and then reclaim the memory: a simple 
  criterion is to wait until all the threads have executed a context switch.
  - used commonly in OS kernels



# Scheduling

The scheduler is responsible for making the decision of which ready thread should run next.

Early batch systems scheduled the next job in line. Later time-sharing systems have many users and processes competing for CPU resources.
Most PCs does not require an efficient CPU scheduling since it has enough CPU resources for its day-to-day tasks. However, servers, smartphones
still require better scheduling due to scarce CPU resources.

Process switching is expensive: a switch from user mode to kernel mode must occur; the state of the current process must be saved; in some systems the memory map must be saved as well; the MMU must be reloaded with the memory map of the new process; the new process must be started; the memory cache and related tables may be invalidated.

Processes may have different bahaviors: 

- CPU-bound 

- I/O-bound

The scheduler must decide whether to run the parent processes or the child processes; which process to run when a process exits; which process to run when a process blocks; which process to run when an I/O interrupt service has finished.

Scheduling algorithms can be divided into two categories depending on how they utilize clock interrupts: 

- _nonpreemptive scheduling_:  a process runs until it blocks or voluntarily releases the CPU;

- _preemptive scheduling_: doing preemptive scheduling requires having a clock interrupt occur at the end of the time interval to give control of the CPU back to the scheduler.

In different environments different scheduling algorithms are needed: 

1. _batch_: still in widespread use in the businesses world for doing payroll, inventory, accounts receivable, accounts payable, interest calculation, claims processing and other periodic tasks. Nonpreemptive algorithms or preemptive algorithms with long time periods for each process are often acceptable.

2. _interactive_: interactive systems; server systems that serve multiple users/clients.

3. _real time_

On all systems, each process should be given a fair share of the CPU (fairness); all parts of the system should be kept busy (balance). 

- batch systems: 
  - _throughput_ 
  - _turnaround time_: the time between submission and termination
  - _CPU utilization_: not a good indicator
  - Common scheduling algorithms:
    - first-come, first-served, blocked processes are put to the end of the queue. Fair, but not ideal for a large number of I/O-bound processes.
    - shortest job first: assuming the run times are known in advance, the mean turnaround time is $(na + (n-1)b + (n-2)c + \cdots + e) / n$.
      optimal (turnaround time) only when all the jobs are available simultaneously; 
      - shortest remaining time next, a preemptive version of shortest job first, where the run time has to be known in advance. This allows a new short job to get   
        good service.

- interactive systems: 
  - _response time_ should be minimized. 
  - _proportionality_: the expected time for a certain task
  - scheduling algorithms:
    - round-robin scheduling: each processs is assigned an equal quantum (equal importance). 
    If the process has blocked or finished before the quantum has elapsed, the CPU switching is done.
    A short quantum causes constant context switches while a long quantum results in poor response time for processes at the end of the waiting list.
    20-50 msec is often reasonable.
    - priority scheduling: processes are assgined priority and the process with the highest priority gets to run first. 
    Changing priority after each clock tick or allocating different quanta to different processes prevents some processes hog the CPU. 
    IO-bound processes may get a higher CPU priority to start its next I/O operation.
    Round robin may be used inside a priority class; 
    Unix `nice` allows a user to voluntarily reduce the priority of his process to be *nice* to other users.
    - multiple queues: priority class 1 gets one quantum, priorty class gets 2 gets two quanta..., whenever a process used up all the quanta allocated to it, it was moved down one class; 
    - shortest process next: make estimates based on past behavior and run the process with the shortest estimated running time; 
    - guaranteed scheduling: make promises to the users about running time and live up to those promises; 
    - lottery scheduling: give processes lottery tickets for various system resources such as CPU time. a lottery ticket is chosen at random, and the process holding that ticket gets the resource. A high-priorty process gets more tickets so more likely to gain CPU time; cooperating processes may exchange tickets if they wish; 
    - fair-share scheduling: each user is allocated some fraction of the CPU and the scheduler picks processes in such a way as to enforce it

- real-time systems: 
  - deadlines should be met, 
  - scheduling should be highly predictable and regular. 
  - Typically, one or more physical devices external to the computer generate stimuli, and the computer must react appropriately to them within a fixed amount of time. The events that a real-time system may have to respond to can be _periodic_ or _aperiodic_. Some periodic events are not _schedulable_ since handling them requires more than the CPU time available. Real-time scheduling algorithms can static (scheduling decisions made before the system starts running) or dynamic (making decisions at runtime)

Policy-mechanism separation is an important idea. A parent can control its children's priority (policy), the scheduling is done by the kernel (mechanism).

# Classical IPC Problems

## The Dining Philosophers

Formulation: five philosophers are seated around a circular table, Each philosopher has a plate of spaghetti. A philosopher needs two forks to eat it. Between each pair of plates is one fork. The life of a philosopher consists of alternating periods of eating and thinking.

The dining philosophers problem is useful for modeling processes that are competing for exclusive access to limited number of resources, such as I/O devices.

_starvation_: all the programs continue to run indefinitely but fail to make any progress.

```c
#include <stdbool.h>

#define N 5
#define LEFT (i+N-1)% N
#define RIGHT (i+1) % N
#define THINKING 0
#define HUNGRY 1
#define EATING 2

typedef int semaphore;
int state[N];
semaphore mutex = 1;
semaphore s[N];         // whether a philosopher has acquired two forks

// philosophers' action, one process per philosopher
void philosopher(int i)
{
        while (true) {
                think();
                take_forks(i);             // acquire two forks and block
                eat();
                put_forks(i);
        }
}

void take_forks(int i)
{
        down(&mutex);
        state[i] = HUNGRY;
        test(i);
        up(&mutex);
        down(&s[i]);    // blocks if forks were not acquired
}

void put_forks(int i)
{
        down(&mutex);
        state[i] = THINKING;
        test(LEFT);     // see if the left neighbor can now eat
        test(RIGHT);
        up(&mutex);
}

void test(int i)
{
        if (state[i] == HUNGRY &&
            state[LEFT] != EATING &&
            state[RIGHT] != EATING) {
                state[i] = EATING;
                up(&s[i]);
        }
}
```

## The Readers and Writers Problem

The readers and writers problem models access to a database. 

It is acceptable to have multiple processes reading the database at the same time, but if one process is updating/writing the database, no other processes may have access to the database, not even readers.

```c
#include <stdbool.h>

typedef int semaphore;
semaphore mutex = 1;    // controls access to rc
semaphore db = 1;       // controls access to the database
int rc = 0;     // reader counter


void reader(void)
{
        while (true) {
                down(&mutex);
                rc = rc + 1;
                if (rc == 1) down(&db);
                up(&mutex);

                read_data_base();

                down(&mutex);
                rc = rc - 1;
                if (rc == 0) up(&db);
                up(&mutex);

                use_data_read();
        }
}

void writer(void)
{
        while (true) {
                think_up_data();
                down(&db);
                write_data_base();
                up(&db);
        }
}
```
