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

## Blocking Queues

Many threading problems can be formulated elegantly and safely by using one or more queues. Producer threads insert items into the queue, and consumer threads retrieve them. Instead of having every thread transfer data, insert transfer object into a queue and let one thread handlle the transfers, no synchronization needed.

A blocking queue causes a thread to block when you try to add an element when the queue is currently full or to remove an element when the queue is empty. Blocking queues are a useful tool for coordinating the work of multiple threads. Worker threads can periodically deposit intermediate results into a blocking queue. Other worker threads remove the intermediate results and modify them further. The queue automatically balances the workload.

Use `put` and `take` when using the queue as a thread management tool.

The `java.util.concurrent` package supplies several variations of blocking queues. By default, the `LinkedBlockingQueue` has no upper bound on its capacity, but a maximum capacity can be optionally specified. The `LinkedBlockingDeque` is a double-ended version. The `ArrayBlockingQueue` is constructed with a given capacity and an optional parameter to require fairness. The `PriorityBlockingQueue` is a priority queue, not a first-in/first-out queue. Elements are removed in order of their priority. A DelayQueue contains objects that implement the `Delayed` interface. Elements can only be removed from a DelayQueue if their delay has elapsed. Java 7 adds a `TransferQueue` interface that allows a producer thread to wait until a consumer is ready to take on an item.


## Efficient Maps, Sets and Queues

The java.util.concurrent package supplies efficient implementations for maps, sorted sets, and queues: `ConcurrentHashMap`, `ConcurrentSkipListMap`, `ConcurrentSkipListSet`, and `ConcurrentLinkedQueue`. These collections return _weakly consistent iterators_, meaning that these iterators may not reflect all modifications that are made after they were constructed. The concurrent hash map can efficiently support a large number of readers and a fixed number of writers.

To atomically update an entry of a map, use `compute` method:

```java
map.compute(word, (k, v) -> v == null ? 1 : v + 1);
```

Also, there are `computeIfPresent` and `computeIfAbsent`.

```java
map.computeIfAbsent(word, k -> new LongAdder()).increment(); // lazy evaluation
```

The `.merge()` has a parameter for the initial value if not present in the map. Otherwise the function passed is called, combining the existing value and the initial value.

```java
map.merge(word, 1L, Long::sum);
```

The Java API provides bulk operations on concurrent hash maps that can safely execute even while other threads operate on the map. The bulk operations traverse the map and operate on the elements they find as they go along. 

- `search`

- `reduce`: combines all keys and/or value, using a provided accumulation function

- `forEach`: applies a function to all keys and/or values

It is possible to specify a parallelism threshold so that the bulk operation is parallelized when the map contains more elements than the threshold.

```java
String result = map.search(threshold, (k, v) -> v > 1000 ? k : null);
map.forEach(threshold, (k, v) -> k + " -> " + v, System.out::println);
Long sum = map.reduceValues(threshold, Long::sum);

Integer maxlength = map.reduceKeys(threshold,
   String::length, // transformer
   Integer::max); // accumulator
```

There is no `ConcurrentHashSet` class, The static `newKeySet` method yields a `Set<K>` that is actually a wrapper around a `ConcurrentHashMap<K, Boolean>`. (All map values are `Boolean.TRUE`).

```java
Set<String> words = ConcurrentHashMap.<String>newKeySet();
```

The `CopyOnWriteArrayList` and `CopyOnWriteArraySet` are thread-safe collections in which all mutators make a copy of the underlying array so that iterators have consistent view that it can access without any synchronization expense.

The `Arrays` class has a number of parallelized operations. The static `Arrays.parallelSort` method can sort an array of primitive values or objects. The `parallelSetAll` method fills an array with values that are computed from a function. `parallelPrefix` method replaces each array element with the accumulation of the prefix for a given associative operation.

# Tasks and Thread Pools

Constructing a new thread is somewhat expensive because it involves interaction with the operating system. If your program creates a large number of short-lived threads, you should not map each task to a separate thread, but use a _thread pool_ instead. A thread pool contains a number of threads that are ready to run. You give a `Runnable` to the pool, and one of the threads calls the run method. When the run method exits, the thread doesn’t die but stays around to serve the next request.

## `Callable`s and `Future`s

A `Runnable` encapsulates a task that runs asynchronously. A `Callable` is similar to a `Runnable` but returns a value.

```java
public interface Callable<V>
{
   V call() throws Exception;
}
```

A `Future` holds the result of an asynchronous computation. A computation is started and the `Future` is given. The owner of the `Future` object can obtain the result when it is ready.

```java
V get()                             // blocks until the computation is finished
V get(long timeout, TimeUnit unit)  // blocks until the computation is finished or throws a TimeoutExecutation if timed out before the computation finished.
void cancel(boolean mayInterrupt)   // 
boolean isCancelled()
boolean isDone()
```

If the running thrad is interrupted, both `get`s throw an `InterruptedException`. Also, it is possible to `cancel` the computation. Canceling a task involves two steps. The underlying thread must be located and interrupted. And the task implementation (in the call method) must sense the interruption and abandon its work. If a `Future` object does not know on which thread the task is executed, or if the task does not monitor the interrupted status of the thread on which it executes, cancellation will have no effect.

One way to execute a `Callable` is to use a `FutureTask`, which implements both the `Future` and `Runnable` interfaces:

```java
Callable<Integer> task = . . .;
var futureTask = new FutureTask<Integer>(task);
var t = new Thread(futureTask); // it's a Runnable
t.start();
. . .
Integer result = task.get(); // it's a Future
// somewhat like a std::packaged_task in C++ except that Future is not explictly got.
```

## Executors

The `Executors` class has a number of static factory methods for constructing thread pools.
