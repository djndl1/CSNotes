Multithreading is extremely useful in practice. Graphical user interface (GUI) programs have a separate thread for gathering user interface events from the host operating environment

# Thread States

Threads can be in one of six states:

- new: created but not yet running. A certain amount of bookkeeping needs to be done before a thread can run.

- runnable: after invoding `.start()`, the thread is in the runnable state. A runnable thread may or may not actually be running. A thread can `yield` its control.

- blocked: when a thread tries to acquire an intrinsic object lock and the lock is currently held by another thread, it becomes blocked.

- waiting: when a thread waits for another thread to notify the scheduler of a condition

- timed waiting: several methods have a timeout parameter.

- terminated: the `run` method exits normally or an uncaught exception terminates the `run` method.

When a thread is blocked or waiting (or, of course, when it terminates), another thread will be scheduled to run. When a thread is reactivated (for example, because its timeout has expired or it has succeeded in acquiring a lock), the scheduler checks to see if it has a higher priority than the currently running threads. If so, it preempts one of the current threads and picks a new thread to run.

```bash
    +--------+
    |        |
    |  new   |                                +---------+
    |        |                                |         |
    +---|----+     acquiring a lock           |  bloked |
        | +---------------------------------->+         |
        | |  +-------<------------------------|---------+
start   | ^  |   lock acquired
        | |  |
        | |  |
        | |  v
    +---v-|--+                                +---------+
    |        | waiting for notification       |         |
    |runnable+------------------------------> | waiting |
    |        |    notification occurred       |         |
    +---|--|-+ <------------------------------|---------+
        |  ^ |
        |  | |  waiting for timeout or notification
run     |  | +------------------------------>+---------+
method  |  |                                 |  timed  |
exits   |  +---------------------------------+         |
        | timeout or notification occurred   | waiting |
        v                                    +---------+
    +---|------+
    |          |
    |terminated|
    |          |
    +----------+

```

# Thread Properties

## Interrupting

the `interrupt` method can be used to request termination of a thread. When the `interrupt` method is called on a thread, the _interrupted status_ of the thread is set. Each thread should occasionally check whether it has been interrupted.

```java
while (!Thread.currentThread().isInterrupted() && more work to do)
{
   do more work
}
```

When the `interrupt` method is called on a thread that blocks on a call such as `sleep` or `wait`, the blocking call is terminated by an `InterruptedException`.

There is no language requirement that a thread which is interrupted should terminate. Interrupting a thread simply grabs its attention. The interrupted thread can decide how to react to the interruption. Some threads are so important that they should handle the exception and continue. But quite commonly, a thread will simply want to interpret an interruption as a request for termination

```java
Runnable r = () -> {
   try {
      . . .
      while (!Thread.currentThread().isInterrupted() && more work to do)
      {
         do more work
      }
   }
   catch(InterruptedException e) {
      // thread was interrupted during sleep or wait
   }
   finally {
      cleanup, if required
   }
   // exiting the run method terminates the thread
};
```

 if your loop calls `sleep`, don’t check the interrupted status. Instead, catch the exception.

## Daemon Threads

`.setDaemon(true)`. A daemon is simply a thread that has no other role in life than to serve others. Examples are timer threads that send regular “timer ticks” to other threads or threads that clean up stale cache entries. When only daemon threads remain, the virtual machine exits. There is no point in keeping the program running if all remaining threads are daemons.

## Handlers for Uncaught Exceptions

The `run` method of a thread cannot throw any checked exceptions, but it can be terminated by an unchecked exception. In that case, the thread dies. Before the thread dies, the exception is passed to a handler for uncaught exceptions. The handler implements `Thread.UncaughtExceptionHandler`. If you don't install a handler for an individual thread, the handler is the thread's `ThreadGroup` object (A thread group is a collection of threads that can be managed together).

## Thread Priorities

A thread inherits the priority of the thread that constructed it. You can increase or decrease the priority of any thread with the setPriority method. You can set the priority to any value between `MIN_PRIORITY` (defined as 1 in the Thread class) and `MAX_PRIORITY` (defined as 10). `NORM_PRIORITY` is defined as 5.

Thread priorities are highly system-dependent. When the virtual machine relies on the thread implementation of the host platform, the Java thread priorities are mapped to the priority levels of the host platform, which may have more or fewer thread priority levels.

Thread priorities may have been useful in early versions of Java that didn’t use operating systems threads. You should not use them nowadays.


# Synchronization

The problem is that access to shared data is not atomic, it can be interrupted in the middle.

## Lock Objects

`ReentrantLock`: It is critically important that the unlock operation is enclosed in a finally clause. If the code in the critical section throws an exception, the lock must be unlocked. Otherwise, the other threads will be blocked forever. 

```java
myLock.lock();
try {
// critical section
} finally {
myLock.unlock();
}
```

The lock is called reentrant because a thread can repeatedly acquire a lock that it already owns. The thread has to call unlock for every call to lock in order to relinquish the lock. Because of this feature, code protected by a lock can call another method that uses the same locks. See the [motivation behind it](https://en.wikipedia.org/wiki/Reentrant_mutex).

## Condition Object/Variables

Often, a thread enters a critical section only to discover that it can’t proceed until a condition is fulfilled. Use a condition object to manage threads that have acquired a lock but cannot do useful work.

```java
// inferior solution

public void transfer(int from, int to, int amount) {
   bankLock.lock();
   try {
      while (accounts[from] < amount) {
         // unlock and wait
         . . .
      }
      // transfer funds
      . . .
   }
   finally {
      bankLock.unlock();
p   } 
```

 A lock object can have one or more associated condition. There is an essential difference between a thread that is waiting to acquire a lock and a thread that has called await. Once a thread calls the await method, it enters a wait set for that condition. The thread is not made runnable when the lock is available. Instead, it stays deactivated until another thread has called the `signalAll` method on the same condition. When a thread calls `await`, it has no way of reactivating itself.

`signal()` notifies one thread, which can be dangerous since if this thread's condition has not been satisfied, the whole system deadlocks. 

```java
   public void transfer(int from, int to, double amount) throws InterruptedException
   {
      bankLock.lock();
      try {
           while (accounts[from] < amount)
              sufficientFunds.await();
           System.out.print(Thread.currentThread());
           accounts[from] -= amount;
           System.out.printf(" %10.2f from %d to %d", amount, from, to);
           accounts[to] += amount;
           System.out.printf(" Total Balance: %10.2f%n", getTotalBalance());
           sufficientFunds.signalAll();
        }
        finally {
           bankLock.unlock();
        }
     }
```

Every object in Java has an intrinsic lock. If a method is declared with the `synchronized` keyword, the object’s lock protects the entire method. That is, to call the method, a thread must acquire the intrinsic object lock. The instrinsic lock has a single associated condition.

```java
class Bank
{
   private double[] accounts;
   public synchronized void transfer(int from, int to, int amount) 
         throws InterruptedException {
      while (accounts[from] < amount)
         wait(); // wait on intrinsic object lock's single condition
      accounts[from] -= amount;
      accounts[to] += amount;
      notifyAll(); // notify all threads waiting on the condition
   }
   public synchronized double getTotalBalance() { . . . }
}
```

It is also legal to declare static methods as synchronized. If such a method is called, it acquires the intrinsic lock of the associated class object. This locks the class object.

Do not use lock, conditions or `synchronized` if possible. There are other mechanism in `java.util.concurrent`. Use `synchronized` first if sufficient. Use Lock/Condition for additional power.

Another way to use the intrinsic lock is to use the synchronized block.

```java
synchronized (obj) {
// critical section
}
```

It's possible to use an ad-hoc lock

```java
public class Bank
{
   private double[] accounts;
   private var lock = new Object();
   . . .
   public void transfer(int from, int to, int amount) {
      synchronized (lock) // an ad-hoc lock {
         accounts[from] -= amount;
         accounts[to] += amount;
      }
      System.out.println(. . .);
   }
}
```

## The Monitor Concept

A monitor, with all its fields being private, has an associated lock, which locks all methods in the class, and can have any number of associated conditions. The Java designer loosely adapted the monitor concept. Every object in Java has an intrinsic lock and an intrinsic condition. If a method is declared with the synchronized keyword, it acts like a monitor method. The condition variable is accessed by calling `wait`/`notifyAll`/`notify`.


## `volatile` and `final`

Computers with multiple processors can temporarily hold memory values in registers or local memory caches. As a consequence, threads running in different processors may see different values for the same memory location! Compilers can reorder instructions for maximum throughput. Compilers won’t choose an ordering that changes the meaning of the code, but they make the assumption that memory values are only changed when there are explicit instructions in the code. However, a memory value can be changed by another thread. 

_Compilers are required to respect locks by flushing local caches as necessary and not inappropriately reordering instructions_. The `volatile` keyword offers a lock-free mechanism for synchronizing access to an instance field.

```java
private volatile boolean done;
public boolean isDone() { return done; }
public void setDone() { done = true; }
```

The compiler will insert the appropriate code to ensure that a change to the done variable in one thread is visible from any other thread that reads the variable. It does not provide any atomicity. You can declare shared variables as volatile provided you perform no operations other than assignment.

```java
final var accounts = new HashMap<String, Double>();
```

Other threads get to see the accounts variable after the constructor has finished. Without using `final`, there would be no guarantee that other threads would see the updated value of accounts—they might all see null, not the constructed `HashMap`.

## Atomics

There are a number of classes in the `java.util.concurrent.atomic` package that use efficient machine-level instructions to guarantee atomicity of other operations without using locks. There are methods for atomically setting, adding, and subtracting values, but if you want to make a more complex update, you have to use the `compareAndSet` method.

```java
largest.updateAndGet(x -> Math.max(x, observed));
```

When multiple threads update a common sum that is used only for later use not for synchronization, use `LongAdder` to avoid high contention. Multiple threads can update different summands, and new summands are automatically provided when the number of threads increases.

```java
var adder = new LongAdder();
for (. . .) 
   pool.submit(() -> {
      while (. . .) {

         . . .
         if (. . .) adder.increment(); 
      }
   });
. . . 
long total = adder.sum();
```

The `LongAccumulator` generalizes this idea to an arbitrary accumulation operation. In the constructor, you provide the operation, as well as its neutral element. When accumulate is called with value v, then one of them is atomically updated as `ai = ai op v`, where `op` is the accumulation operation written in infix form.

## Deadlock

Unsatisfied conditions blocks all threads and the program eventually hang. Unfortunately, there is nothing in the Java programming language to avoid or break these deadlocks. You must design your program to ensure that a deadlock situation cannot occur.

## Thread-Local Variables

Sometimes it is possible to avoid sharing by giving each thread its own instance, using the `ThreadLocal` helper class.

```local
public static final ThreadLocal<SimpleDateFormat> dateFormat
   = ThreadLocal.withInitial(() -> new SimpleDateFormat("yyyy-MM-dd"));

String dateStamp = dateFormat.get().format(new Date());
```

The first time you call get in a given thread, the lambda in the constructor is called. From then on, the get method returns the instance belonging to the current thread.

# Thread-Safe Collections

You can protect a shared data structure by supplying a lock, but it is usually easier to choose a thread-safe implementation instead.
