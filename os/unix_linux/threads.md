# Concept

All threads within a single process have access to the same process components, such as file descriptors and memory. We can simplify code that deals with asynchronous events by assigning a separate thread to handle each event type. The overall throughput may be improved and interactive programs can realize improved response time by using multiple threads to separpate to the portions of the program that deal with user input and output from the other parts of the program.

A thread consists of the information necessary to represent an execution context within a process, including a thread ID, a set of registers values, a stack, a scheduling priority and policy, a signal mask, an `errno` variable and thread-specific ddata. Everything with a process is sharable among the threads in a process, including the text of executable program, the program's global and heap memory, the stacks, and the file descriptors.

# Thread Identification

A thread ID is represented by the `pthread_t` data type, which may not be an integer (it is an `unsigned long` on linux but is assigned a pointer to the underlying `struct pthread`) and provides no portable way to print its value for debugging. `pthread_equal()` compares two thread IDs and `pthread_self()` obtains the current thread ID. Thread IDs may be used to assign a job to a particular thread.

# Thread Creation

When a program runs, it starts out as a single process with a single thread of control until it creates  more threads of control by `pthread_create()`. If more than one argument to the function is needed, store them in a struct and pass the pointer. pthread functions usually return an error code when they fail. They don't set `errno` like other POSIX functions. The new thread cannot safely use the `thread_t` that handles it because the spawning thread may not have returned from `pthread_create()` thus the `thread_t` may not usable.

# Thread Termination

If any thread within a process calls `exit`, `_Exit` or `_exit`, the entire process terminates. A signal sent to a thread will terminate the entire process. To terminate a thread, a thread can simply return, be canceled by another thread or call `pthread_exit`. `pthread_join` gets the returned value from `pthread_exit` or the simple `return`ed value or the `PTHREAD_CANCEL`.


The typeless pointer passed to `pthread_create` and `pthread_exit` can be used to pass more than a single value through a `struct`.

One thread can request that another in the same process be canceled by calling  `pthread_cancel`. The default behavior is that the thread requested to exit `pthread_exit`s with `PTHREAD_CANCELED`. A thread can ignore or otherwise control how it is canceled. A thread can arrange for functions to be called when it exits, known as _thread cleanup handlers_, recorded in stack. The `pthread_cleanup push` schedules the cleanup function when the thread `pthread_exit`, responds to a cancellation request or makes a call to `pthread_cleanup_pop` with nonzero execute argument. A zero argument to `pthread_cleanup_push` merely remove a handler. A simple `return` does not trigger cleanup handlers. Returning between `pthread_cleanup_push` and `pthread_cleanup_pop` is undefined behavior (on linux they are implemented as paired macros that expand to text containing `{` and `}`). The only portable way to return in between is to call `pthread_exit`.

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

The mutual-exclusion mechanism works only if we design our threads to follow the same data-aceess rules. If we allow one thread to access a shared resource without first acquiring a lock, t hen inconsistencies can occur even though the rest of our threads do acquire the lock before attempting to access the shared resource.

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
        }
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

Deadlocks may occur if a thrad tries to lock the same mutex twice or two threads holding one of two mutexes respectively and try to acquire the other. Lock ordering may prevent deadlock. If it is impossible to arrange a lock ordering, use `pthread_trylock` to acquire another lock, release the already-held lock if failed.

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
                pthread_mutex_unlock(&hashlock);
                // continue intialization
                pthread_mutex_unlock(&fp->f_lock);
        }
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
                if (tfp == fp) {
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
                // it's gonna be destroyed anyway
                free(fp);
        } else {
                fp->f_count--;
                pthread_mutex_unlock(&fp->f_lock);
        }
}
```

## Timed lock

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
