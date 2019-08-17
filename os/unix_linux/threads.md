# Concept

All threads within a single process have access to the same process components, such as file descriptors and memory. We can simplify code that deals with asynchronous events by assigning a separate thread to handle each event type. The overall throughput may be improved and interactive programs can realize improved response time by using multiple threads to separate the portions of the program that deal with user input and output from the other parts of the program.

A thread consists of the information necessary to represent an execution context within a process, including a thread ID, a set of registers values, a stack, a scheduling priority and policy, a signal mask, an `errno` variable and thread-specific data. Everything with a process is sharable among the threads in a process, including the text of executable program, the program's global and heap memory, the stacks, and the file descriptors.

# Thread Identification

A thread ID is represented by the `pthread_t` data type, which may not be an integer (it is an `unsigned long` on linux but is assigned a pointer to the underlying `struct pthread`) and provides no portable way to print its value for debugging. `pthread_equal()` compares two thread IDs and `pthread_self()` obtains the current thread ID. Thread IDs may be used to assign a job to a particular thread.

# Thread Creation

When a program runs, it starts out as a single process with a single thread of control until it creates  more threads of control by `pthread_create()`. If more than one argument to the function is needed, store them in a struct and pass the pointer. pthread functions usually return an error code when they fail. They don't set `errno` like other POSIX functions. The new thread cannot safely use a global `thread_t` that identifies this thread because the spawning thread may not have returned from `pthread_create()` thus the `thread_t` may not usable.

# Thread Termination

If any thread within a process calls `exit`, `_Exit` or `_exit`, the entire process terminates. A signal sent to a thread will terminate the entire process. To terminate a thread, a thread can simply _return_, _be canceled_ by another thread or call `pthread_exit`. `pthread_join` gets the returned value from `pthread_exit` or the simple `return`ed value or the `PTHREAD_CANCEL`.


The typeless pointer passed to `pthread_create` and `pthread_exit` can be used to pass more than a single value through a `struct`.

One thread can request that another in the same process be canceled by calling  `pthread_cancel`. The default behavior is that the thread requested to exit `pthread_exit`s with `PTHREAD_CANCELED`. A thread can ignore or otherwise control how it is canceled. A thread can arrange for functions to be called when it exits, known as _thread cleanup handlers_, recorded in stack. The `pthread_cleanup_push` schedules the cleanup function when the thread `pthread_exit`, responds to a cancellation request or makes a call to `pthread_cleanup_pop` with nonzero execute argument. A zero argument to `pthread_cleanup_push` merely remove a handler. A simple `return` does not trigger cleanup handlers. Returning between `pthread_cleanup_push` and `pthread_cleanup_pop` is undefined behavior (on linux they are implemented as paired macros that expand to text containing `{` and `}`). The only portable way to return in between is to call `pthread_exit`.

```c
#  define pthread_cleanup_push(routine, arg) \
  do {									      \
    __pthread_cleanup_class __clframe (routine, arg)

/* Remove a cleanup handler installed by the matching pthread_cleanup_push.
   If EXECUTE is non-zero, the handler function is called. */
#  define pthread_cleanup_pop(execute) \
    __clframe.__setdoit (execute);					      \
 
 } while (0)
```

A thread's underlying storage can be reclaimed immediately on termination if the thead has been detached without the need for another thread to join with the terminated thread.

# Thread Synchronization

When mutliple threads of control share the same memory, we need to make sure that each thread sees a consistent view of its data. To solve the problem, the threads have to use a lock that will allow only one thread to access the variable at a time. If the modification is atomic, then there isn't a race. If our data always appears to be _sequentially consistent_, then no synchronization is needed.  In modern computer systems, memory accesses take multiple bus cycles, and multiprocessors generally interleave bus cycles among multiple processors, so we aren’t guaranteed that our data is sequentially consistent.

## Mutexes

We can protect our data and ensure access by only one thread at a time by using the pthreads mutual-exclusion interfaces. A _mutex_ is basically a lock that we set before accessing a shared resources and release (unlock) when we're done. While it is set, any other thread that tries to set it will block until we release it. 

The mutual-exclusion mechanism works only if we design our threads to follow the same data-aceess rules. If we allow one thread to access a shared resource without first acquiring a lock, then inconsistencies can occur even though the rest of our threads do acquire the lock before attempting to access the shared resource.

A mutex variable is represented by the `pthread_mutex_t` dat type. Initialize a mutex by

- setting it to `PTHREAD_MUTEX_INITIALIZER` if it is statisically allocated;

- calling `pthread_mutex_init`.

`pthread_mutex_destroy()` is neede before freeing the memory if a mutex is dynamically allocated.

`pthread_mutex_lock` locks a mutex. `pthread_mutex_unlock` unlocks a mutex. If a thread cannot afford to block, it can use `pthread_mutex_tryblock` to lock the mutex conditionally.

```c
#include <pthread.h>
#include <stdlib.h>

// a dynamically allocated object
struct foo {
        int f_count;            // reference count to ensure that memory is not freed before all threads are done using it.
        pthread_mutex_t f_lock; // protect in multithreading
        int f_id;
        // more stuff can go here
};

struct foo *foo_alloc(int id)
{
        struct foo *fp;

        if ((fp = malloc(sizeof(struct foo))) != NULL) {
                fp->f_count = 1; 
                fp->f_id = id;
                if (pthread_mutex_init(&fp->f_lock, NULL) != 0) {
                        free(fp);
                        return (NULL);
                }
        } else
                return NULL;
        return fp;
}

void foo_hold(struct foo *fp)
{
        pthread_mutex_lock(&fp->f_lock);
        fp->f_count++;
        pthread_mutex_unlock(&fp->f_lock);
}

void foo_rele(struct foo *fp)
{
        pthread_mutex_lock(&fp->f_lock);
        // release the reference count when a thread is done
        if (--fp->f_count == 0) {
                // no need to keep the object when no thread is using it.
                pthread_mutex_unlock(&fp->f_lock);
                pthread_mutex_destroy(&fp->f_lock);
                free(fp);
        } else {
                pthread_mutex_unlock(&fp->f_lock);
        }
}
```

## Deadlock avoidance

Deadlocks may occur if a thread tries to lock the same mutex twice or two threads holding one of two mutexes respectively and try to acquire the other. Lock ordering may prevent deadlock. If it is impossible to arrange a lock ordering, use `pthread_trylock` to acquire another lock, release the already-held lock if failed.

```c
#include <pthread.h>
#include <stdlib.h>

#define NHASH 29
#defin HASH(id) (((unsigned long)id)%NHASH)

struct foo *fh[NHASH]; // a hash table keeping foos

pthread_mutex_t hashlock = PTHREAD_MUTEX_INITIALIZER;

struct foo {
        int               f_count;
        pthread_mutex_t   f_lock;
        int               f_id;
        struct foo       *f_next;
        /* more stuff go here */
};

struct foo *foo_alloc(int id)
{
        struct foo *fp;
        int        idx;

        if ((fp = malloc(sizeof(struct foo))) != NULL) {
                fp->f_count = 1;
                fp->f_id = id;
                if (pthread_mutex_init(&fp->f_lock, NULL) != 0) {
                        free(fp);
                        return NULL;
                }
                idx = HASH(id);
                pthread_mutex_lock(&hashlock);
                fp->f_next = fh[idx]; // put fp at the beginning of the bucket
                fh[idx] = fp;
                pthread_mutex_lock(&fp->f_lock);
                // continue intialization
                pthread_mutex_unlock(&hashlock);
                pthread_mutex_unlock(&fp->f_lock);
        } else
                return NULL;
        return fp;
}

void foo_hold(struct foo *fp)
{
        pthread_mutex_lock(&fp->f_lock);
        fp->f_count++;
        pthread_mutex_unlock(&fp->f_lock);
}

struct foo* foo_find(int id)
{
        struct foo *fp;

        pthread_mutex_lock(&hashlock);
        for (fp = fh[HASH(id)]; fp != NULL; fp = fp->next) {
                if (fp->f_id == id) {
                        foo_hold(fp);
                        break;
                }
        }
        pthread_mutex_unlock(&hashlock);
        return fp;
}

void foo_rele(struct foo *fp)
{
        struct foo *tfp;
        int         idx;

        pthread_mutex_lock(&hashlock);
        if (--fp->f_count == 0) { // last release
                idx = HASH(fp->f_id);
                tfp = fh[idx];
                if (tfp == fp) {            // if at the head of the bucket
                        fh[idx] = fp->f_next;
                } else {
                        while (tfp->f_next != fp)
                                tfp = tfp->f_next;
                        tfp->f_next = fp->f_next;
                }
                pthread_mutex_unlock(&hashlock);
                pthread_mutex_destroy(&fp->f_lock); 
                // we are not accessing any members of fp
                // hashlock cannot prevent anyone from holding fp and modify it directly without accessing the hash table
                // it's not in the hash table and it's gonna be destroyed anyway.
                free(fp);
        } else {
                fp->f_count--;
                pthread_mutex_unlock(&fp->f_lock);
        }
}
```

`pthread_mutex_timedlock` returns the error code `ETIMEOUT` when it failed to lock the mutex after tiemout value is reached. It can be used to avoid blocking indefinitely.

## Reader-Writer Locks (shared-exclusive locks)

Three states are possible with a reader-writer lock: locked in _read mode_, locked in _write mode_ and _unlocked_. Only one thread at a time can hold a reader-writer lock in write mode, but multiple threads can hold a reader-writer lock in read mode at the same time. 

Reader-write locks are well suited for situations in which data structures are read more often than they are modified.

reader-writer locks must be initializerd before use and destroyed before freeing their underlying memory.

Implementations might place a limit on the number of times a reader–writer lock can be locked in shared mode, so we need to check the return value of `pthread_rwlock_rdlock`.

```c
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

struct job {
        struct job     *j_next;
        struct job     *j_prev;

        pthread_t       j_id; // tells which thread handles the job
};

struct queue {
        struct job              *q_head;
        struct job              *q_tail;
        pthread_rwlock_t         q_lock;
};

int queue_init(struct queue *qp)
{
        int err;

        qp->q_head = qp->q_tail = NULL;
        err = pthread_rwlock_init(&qp->q_lock, NULL);
        if (err != 0)
                return (err);
        // continue initialization
        return 0;
}

// insert at the head of the queue
void job_insert(struct queue *qp, struct job *jp)
{
        pthread_rwlock_wrlock(&qp->q_lock);
        jp->j_next = qp->q_head;
        jp->j_prev = NULL;
        if (qp->q_head != NULL)
                qp->q_head->j_prev = jp;
        else
                qp->q_tail = jp;
        qp->q_head = jp;
        pthread_rwlockattr_destroy(&qp->q_lock);
}

// append a job on the tail of queue
void job_append(struct queue *qp, struct job *jp)
{
        pthread_rwlock_wrlock(&qp->q_lock);
        jp->j_next = NULL;
        jp->j_prev = qp->q_tail;
        if (qp->q_tail != NULL)
                qp->q_tail->j_next = jp;
        else
                qp->q_head = jp;
        qp->q_tail = jp;
        pthread_rwlock_unlock(&qp->q_lock);
}

// remove the given job from a queue
void job_remove(struct queue *qp, struct job *jp)
{
        pthread_rwlock_wrlock(&qp->q_lock);
        if (jp == qp->q_head) {
                qp->q_head = jp->j_next;
                if (qp->q_tail == jp)           // if jp is the only job in the queue
                        qp->q_tail = NULL;
                else
                        jp->j_next->j_prev = jp->j_prev; // acutally NULL
        } else if (jp == qp->q_tail) {
                qp->q_tail = jp->j_prev;
                jp->j_prev->j_next = jp->next; // actually NULL
        } else {
                jp->j_prev->j_next = jp->j_next;
                jp->j_next->j_prev = jp->j_prev;
        }
        pthread_rwlock_unlock(&qp->q_lock);
}

// find a job for the given thread ID
struct job *job_find(struct queue *qp, pthread_t id)
{
        struct job *jp;

        if (pthread_rwlock_rdlock(&qp->q_lock) != 0)
                return NULL;

        for (jp = qp->q_head; jp != NULL; jp = jp->j_next)
                if (pthread_equal(id, jp->j_id))
                        break;

        pthread_rwlock_unlock(&qp->q_lock);
        return jp;
}
```

The SUS provides functions to lock reader-writer locks with a timeout to give applications a way to avoid blocking indefinitely while trying to acquire a reader-writer lock: `pthread_rwlock_timedrdlock`, `pthread_rwlock_timedwrlock`.

### Conditional Variables 

When used with mutexes, condition variables allow threads to wait in a race-free way for arbitrary conditions to occur. The condition itself is protected by a mutex. A thread must first lock the mutex to change the condition state. Other threads will not notice the change until they acquire the mutex.

A condition variable `pthread_cont_t` can initialized by `pthread_cond_t`. A statistically allocated `pthread_cond_t` is initialized by `PTHREAD_COND_INITIALIZER`. We use `pthread_cond_destroy` to deinitialize a condition variable before freeing its underlying memory. We use `pthread_cond_wait` to wait for a condition to be true, A timed variant is provided to return an error code if the condition hasn't been satisfied in the specified amount of time. The wait operation atomically unlocks the mutex and waits for the signal. There are two functions to notify that a condition has been satisfied: `pthread_cond_signal` and `pthread_cond_broadcast`.


```c
#include <pthread.h>

struct msg {
        struct msg *m_next;
        /* more stuff go here */
};

struct msg *workq;

pthread_cond_t qready = PTHREAD_COND_INITIALIZER;

pthread_mutex_t qlock = PTHREAD_MUTEX_INITIALIZER;

void process_msg(void)
{
        struct msg *mp;

        for (;;) {
                pthread_mutex_lock(&qlock);
                while (workq == NULL)
                        pthread_cond_wait(&qready, &qlock);
                mp = workq;
                workq = mp->m_next;
                pthread_mutex_unlock(&qlock);
        }
}

void enqueue_msg(struct msg *mp)
{
        pthread_mutex_lock(&qlock);
        mp->m_next = workq;
        workq = mp;
        pthread_mutex_unlock(&qlock);
        pthread_cond_signal(&qready); 
        /* there is a while loop checking the condition so it's okay
        to signal after we release the lock. a thread wakes up and find
        the condition has been changed by some other thread that is not 
        waiting and go back waiting */
}
```

Condition variables are a inter-thread synchronization mechanism, not the predicate condition itself. The boolean condition must be examined. In general, whenever a condition wait returns, the thread has to re-evaluate the predicate associated with the condition wait to determine whether it can safely proceed, should wait again, or should declare a timeout. A return from the wait does not imply that the associated predicate is either true or false. It is thus recommended that a condition wait be enclosed in the equivalent of a "while loop" that checks the predicate. Condition variables are for signaling.

The `pthread_cond_broadcast()` or `pthread_cond_signal()` functions may be called by a thread whether or not it currently owns the mutex that threads calling `pthread_cond_wait()` or `pthread_cond_timedwait()` have associated with the condition variable during their waits. If predictable scheduling behavior is required, then that mutex shall be locked by the thread calling `pthread_cond_broadcast()` or` pthread_cond_signal()`. Read Section 3.3.3 of "Programming With POSIX Threads". The `pthread_cond_wait` blocks on the mutex after signalling if the mutex is not available.

https://stackoverflow.com/questions/14924469/does-pthread-cond-waitcond-t-mutex-unlock-and-then-lock-the-mutex


## Spin Locks

The process is blocked by busy-waiting (spinning) until the lock can be acquired. A spin lock could be used in situations where locks are held for short periods of times and threads don't want to incur the cost of being descheduled. Spin locks are often used as low-level primitive to implement other types of locks. Although efficient, they can lead to wasting CPU resources. Spin locks are useful when used in a nonpreemptive kernel, they block interrupts. Spin locks are not as useful unless in a real-time scheduling class that doesn't allow preemption. Many mutex implementations are so efficient that the performance of applications using mutex locks is equivalent to their performance if they had used spin locks. Spin locks are useful in limited circumstances.

The interfaces for spin locks are similar to those for mutexes, making it relatively easy to replace one with the other.

- `pthread_spin_init`; `pthread_spin_destroy`;

- `pthread_spin_lock`; `pthread_spin_trylock`; `pthread_spin_unlock`;

We need to be careful not to call any functions that might sleep while holding the spin lock. If we do, then we’ll waste CPU resources by extending the time other threads will spin if they try to acquire it.

## Barriers

Barriers are a synchronization mechanism that can be coordinate multiple threads working in parallel. A barrier allows each thread to wait until all cooperating threads have reached the same point and then continue executing from there. They allow arbitrary number of threads to wait until all of the threads have completed processing but the threads don't have to exit.

- `pthread_barrier_init`; `pthread_barrier_destroy`;

When we initialize a barrier, we use the count argument to specify the number of threads that must reach the barrier before all of the threads will be allowed to continue.

`pthread_barrier_wait` indicates that a thread is done with its work and is ready to wait for all the other threads to catch up. The thread calling `pthread_barrier_wait` is put to sleep if the count is not satisfied.  If the thread is the last one to call `pthread_barrier_wait`, thereby satisfying the barrier count, all of the threads are awakened.

 When   the  required  number  of  threads  have  called  `pthread_barrier_wait()` specifying the barrier, the constant  `PTHREAD_BARRIER_SERIAL_THREAD`  shall  be  returned  to  one unspecified thread and zero shall be returned to each of the remaining threads.

```c
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <limits.h>
#include <sys/time.h>

#define NTHR 2              // number of threads, better set to `nproc`
#define NUMNUM 8000000L     // number of numbers to sort
#define TNUM (NUMNUM/NTHR)  // number to sort per thread

long nums[NUMNUM];
long snums[NUMNUM];

pthread_barrier_t b;

int complong(const void *p_lhs, const void *p_rhs)
{
        long l1 = *(long*)p_lhs;
        long l2 = *(long*)p_rhs;

        if (l1 == l2)
                return 0;
        else if (l1 < l2)
                return -1;
        else
                return 1;
}

void *thr_fn(void *arg)
{
        long idx = (long)arg;

        qsort(&nums[idx], TNUM, sizeof(long), complong);
        pthread_barrier_wait(&b);

        return ((void*)0);
}

// one of the smallest eight elements of the eight sorted array is the smallest
void merge()
{
        long idx[NTHR];
        long i, minidx, sidx, num;

        for (i = 0; i < NTHR; i++)
                idx[i] = i * TNUM; // keep the smallest eight
        for (sidx = 0; sidx < NUMNUM; sidx++) {
                num = LONG_MAX;
                for (i = 0; i < NTHR; i++) {
                        if ((idx[i] < (i+1)*TNUM) && (nums[idx[i]] < num)) {
                                num = nums[idx[i]];
                                minidx = i;
                        }
                }
                snums[sidx] = nums[idx[minidx]];
                idx[minidx]++;    // update the index of the smallest in corresponding array
        }
}

int main(int argc, char *argv[])
{
        unsigned long i;
        struct timeval start, end;
        long long startusec, endusec;
        double elapsed;
        int err;
        pthread_t tid;

        // generate NUMNUM random long integers
        srandom(1);
        for (i = 0; i < NUMNUM; i++)
                nums[i] = random();

        gettimeofday(&start, NULL);
        pthread_barrier_init(&b, NULL, NTHR + 1); // the main thread included
        for (i = 0; i < NTHR; i++) {
                err = pthread_create(&tid, NULL, thr_fn, (void*)(i * TNUM));
                if (err != 0) {
                        fprintf(stderr, "cannot create thread\n!");
                        exit(1);
                }
        }
        pthread_barrier_wait(&b);
        merge();
        gettimeofday(&end, NULL);

        // print the sorted list
        startusec = start.tv_sec * 1000000 + start.tv_usec;
        endusec = end.tv_sec * 1000000 +  end.tv_usec;
        elapsed = (double) (endusec - startusec) / 1000000.0;
        printf("sort took %.4f seconds\n", elapsed);
        //  for (i = 0; i < NUMNUM; i++)
        //      printf("%ld\n", snums[i]);

        return 0;
}

```

# Thread Limits

The SUS defines several limits associated with the operation of threads, which are queried using `sysconf`:

`PTHREAD_DESTRUCTOR_INTERATIONS`; `PTHREAD_KEYS_MAX`; `PTHREAD_STACK_MIN`; `PTHREAD_THREADS_MAX`.

# Thread Attributes

The pthread interface allows us to fine-tune the behavior of threads and synchronization objects by setting various attributes associated with each object. Each object is associated with its own type of attribute object. The attribute object is opaque to applications. A pair of functions exist to set the attributes to their default values and destroy the attribute object. Each attribute has a pair of functions to get/set the value of the attribute.

We use `pthread_attr_init` to initialize `pthread_attr_t` structure to the default values. `pthread_attr_destroy` set the attribute object with invalid values and free the resource if any.

- `detachstate`: detached thread attribute. `pthread_attr-setdetachstate` set the `detachstate` attribute to one of two legal values: `PTHREAD_CREATE_DETACHED` to start the thread in the detached state or `PTHREAD_CREATE_JOINABLE` to start the thread normally. `pthread_attr_getdetachstate` obtain the current `detachstate` attribute.

- `stackaddr`: lowest address of the thread stack. With threads, the same amount of virtual address space must be shared by all the thread stacks. If virtual address space runs out, `malloc` and `mmap` can allocate space for an alternative stack and `pthread_set_stack` change the stack location of the created threads. For most applications, this is not necessary and should be avoided.

- `stacksize`: minimum size in bytes of thread stack. An application can also get and set the `stacksize` thread attribute using the `pthread_attr_getstacksize` and `pthread_attr_setstacksize`.

- `guardsize`: guard buffer size in bytes at end of thread stack. It controls the size of the memory extent after the end of the thread's stack to protect against stack overflow. A commonly used value is the system page size. If the thread's stack pointer overflows into the guard area, the application will receive an error, possibly with a signal.

Two thread attributes not included in the `pthread_attr_t` structure are the _cancellability state_ (`PTHREAD_CANCEL_ENABLE` or `PTHREAD_CANCEL_DISABLE`) and the `cancelability type`, which affect the behavior of a thread in response to a call to `pthread_cancel`.

A call to `pthread_cancel` doesn’t wait for a thread to terminate. In the default case, a thread will continue to execute after a cancellation request is made until the thread reaches a cancellation point. A cancellation point is a place where the thread checks whether it has been canceled, and if so, acts on the request. POSIX.1 guarantees that cancellation points will occur when a thread calls certain functions. A custom cancellation point can be set by calling `pthread_testcancel`.

A thread starts with a default cancelability state of `PTHREAD_CANCEL_ENABLE`. When the state is set to `PTHREAD_CANCEL_DISABLE`, a call to `pthread_cancel` will not kill the thread. Instead, the cancellation request remains pending for the thread. When the state is enabled again, the thread will act on any pending cancellation requests at the next cancellation point.

The default cancellation type we have been describing is known as _deferred cancellation_. However, `pthread_setcanceltype` can set it to `PTHREAD_CANCEL_ASYNCHRONOUS` type. With asynchronous cancedllation, a thread can be cancelled at any time, rather than when hitting a cancedllation point.

# Synchronization Attributes

## Mutex Attributes

`phtread_mutexattr_t` accepts the default attributes by using `PTHREAD_MUTEX_INITIALIZER` or by calling `pthread_mutex_init` with a null pointer for the argument for the mutex attribute.

There are three attributes of interest

- _process-shared_ attribute: the default is `PTHREAD_PROCESS_PRIVATE`. Access to shared data by multiple processes usually requires synchronization, just as does access to shared data by multiple threads. If the process-shared mutex attribute is set to _PTHREAD_PROCESS_SHARED_, a mutex allocated from a memory extent shared between multiple processes may be used for synchronization by those processes. The process-shared mutex attribute allows the pthread library to provide more efficient mutex implementations when the attribute is set to PTHREAD_PROCESS_PRIVATE, which is the default case with multithreaded applications.

- _robust attribute_: It is meant to address the problem of mutex state recovery when a process terminates while holding a mutex. The default is `PTHREAD_MUTEX_STALLED`, which means that no special action is taken when a process terminates while holding a mutex. The other value is `PTHREAD_MUTEX_ROBUST`. This value will cause a thread blocked in a call to pthread_mutex_lock to acquire the lock when another process holding the lock terminates without first unlocking it. The next owner should call pthread_mutex_consistent(3) on the acquired mutex to make it consistent again before using it any further.

- _type_ attribute: 

1. `PTHREAD_MUTEX_NORMAL`: A standard mutex type that doesn’t do any special error checking or deadlock detection.

2. `PTHREAD_MUTEX_ERRORCHECK `: provides error checking.

3. `PTHREAD_MUTEX_RECURSIVE`: A mutex type that allows the same thread to lock it multiple times without first unlocking it. A recursive mutex maintains a lock count and isn’t released until it is unlocked the same number of times it is locked.

4. `PTHREAD_MUTEX_DEFAULT`: A mutex type providing default characteristics and behavior. Implementations are free to map it to one of the other mutex types.

| Mutex Type               | Relock without unlock | Unlock when unlocked | Unlock when not owned |
| :---:                    | :---:                 | :---:                | :---:                 |
| `PTHREAD_MUTEX_NORMAL`    | deadlock              | undefined            | undefined             |
| `PTHREAD_MUTEX_ERRORCHECK` | returns error         | returns error        | returns error         |
| `PTHREAD_MUTEX_RECURSIVE` | allowed               | returns error        | returns error         |


It is not a good idea to use a recursive mutex with a condvar.

Recursive use TODO

## Reader-Writer Lock Attributes

Reader-writer locks have attributes similar to mutexes. The only attribute supported for reader-writer lock is the _process-shared_ attribute, identical to the mutex process-shared attribute. Implementations arefree to define additional nonstandard ones.

## Condition Variable Attributes

The SUS defines two attributes for condition variables:

- process-shared: It controls whether condition variables can be used by threads within a single process only or from within multiple processes. 

- clock: The clock attribute controls which clock is used when evaluating the timeout argument of the `pthread_cond_timedwait` function.

## Barrier Attributes

The only barrier attribute currently defined is the _process-shared_ attribute, which controls whether a barrier can be used by threads from multiple processes or only from within the process that initialized the barrier.
