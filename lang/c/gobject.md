# Every Basic Type

## GQuark

Use: when both integers and string can be used interchangeably, or used for string interning.

The string-to-int mapping is maintained as a hash table `quark_ht`. The reverse is a one-based (since quarks are non-zero, 0 denotes `NULL`) string array `quarks` (`char **` of course, allocated per `QUARK_BLOCK_SIZE`), the `quark_seq_id`  is the index for the next new quark. A mutex is used for thread-safe access to the quark structures. These structures are initialized at load time of glib. Note the old quark string array is leaked every time the new one replaces the old one.

Multiple duplicated strings are stored in string blocks if small enough, rather than scattered around.

# Concurrency

## Threading

public (`GThread`) -> (`GRealThread`) -> (`GThreadPosix`, containg `pthread_t`) 

public (`g_thread_new`/`g_thread_try_new`) -> (`g_thread_new_internal`) -> (`g_system_thread_new`) -> `pthread_create`

`g_thread_proxy` accepts a `GRealThread` as the data and executes the function inside the 
object. (POSIX Implementation) However, the function pointer is only set after the proxy function starts. The `GRealThread` object is instantiated inside `g_system_thread_new` and the proxy function is started on a new thread. When it started, the `GRealThread` object it accepts is thread-locally stored using a private key `g_thread_specific_private` (triggers `g_thread_cleanup` when destroyed) so that the new thread can retrieve its own handle (using `g_thread_self()`) later (thus gives a refcount of 2, one for the private referencem, the other for the caller), and then suspends due to being unable to acquire the `g_thread_new` mutex. The `GRealThread` object is later set inside `g_thread_new_internal` and then the `g_thread_new` mutex is unlocked so that the proxy function on the new thread can go on and execute the intended thread function. The underlying wrapper around `pthreads` basically just fires the new thread and returns the `GRealThread` handle. The return value of the thread function is stored in the handle. The proxy function returns `NULL`.

For Win32 implementation, a nesting `__stdcall` proxy `g_thread_win32_proxy` is used to execute `g_thread_proxy`, which in turns executes the real thread function. The handle is closed at `g_system_thread_free`.

The `ours` field is to indicate whether the thread is a `GThread`.

For the global `g_thread_specific_private`, by 

> Although the same key value may be used by different threads, the values bound to the key by `pthread_setspecific()` are maintained on a per-thread basis and persist for the life of the calling thread.

> After a TLS index is allocated, each thread of the process can use it to access its own TLS slot for that index. 

## Mutex Locks

On POSIX Systems mutexes are implemented as a thin wrapper around `pthread_mutex_t` (a normal one or an adaptive NP), on Win32, it's a simple `SRWLock`.

`g_*_get_impl` is there to ensure the public API is thread-safe. When GLib type is itself the underlying system handle, no atomicity is required.

## Condition Variable

[Futexes Are Tricky](https://www.researchgate.net/publication/228708140_Futexes_Are_Tricky)
[A Futex Overview and Update](https://lwn.net/Articles/360699/)
[Condition Variables and Futex](https://www.remlab.net/op/futex-condvar.shtml)

They can be implemented on top of the system API. For linux, futex is used. The `wait` operation simply unlocks the mutex and call into `futex` with the sampled futex value (which is atomically incremented to indicate a condition change when `signal`ed) as the expected value. Futex overflow is not likely to be a problem since there is basically no way `signal` would be called billions times in a row.

## Once-Initialization

Implemented without any system initialization API.
`g_once_cond` is there to indicate the initialization state, `g_once_mutex` for checking and writing the once structure.

```c
gpointer
g_once_impl (GOnce       *once,
	     GThreadFunc  func,
	     gpointer     arg)
{
  g_mutex_lock (&g_once_mutex);

  while (once->status == G_ONCE_STATUS_PROGRESS)
    g_cond_wait (&g_once_cond, &g_once_mutex);

  if (once->status != G_ONCE_STATUS_READY)
    {
      // other threads are blocked on the mutex
      once->status = G_ONCE_STATUS_PROGRESS;
      g_mutex_unlock (&g_once_mutex);
      // we can now safely unlock the mutex since the others will sleep on the condvar

      once->retval = func (arg);

      g_mutex_lock (&g_once_mutex); 
      // every other thread should be at the three blocking sites above
      // the extreme case would be that one thread either sleeps on the condvar or 
      // tries to acquire the lock here
      // in any case, one thread will acquire the lock at this line
      once->status = G_ONCE_STATUS_READY;
      g_cond_broadcast (&g_once_cond);
    }

  g_mutex_unlock (&g_once_mutex);
  // Contension for the mutex starts again
  // 1. joins the initializing threads
  // 2. wakes on the signal, either blocks again or unlocks to return the retval

  return once->retval;
}
```

g_once_init_enter/leave are combined to initialize a zero-value only once. The combined 
logic is similar to that of `g_once`, with a linked list as the once-structure to indicate the 
initialization state. The initialization is inside `g_once_init_leave`. This does not, however, prevent a second-time assignment, which could be the same value or a different value.

```c
static gsize initialization_value = 0;

if (g_once_init_enter (&initialization_value))
  {
    gsize setup_value = 42; // initialization code here

    g_once_init_leave (&initialization_value, setup_value);
  }
```

## Thread Local Storage

`GPrivate` should be statically initialized. 

The Win32 implementation employs double-checked locking to once-initialize the key. A linked list of destructor functions are maintained (which can be access from the thread exit function and cannot be protected by the `CriticalSection` lock) to destruct the object since Win32 does not support destructors for an index. `g_thread_win32_thread_detach()` is called in `DllMain` when the thread detaches.

# Error Handling

## `GError`

Used to report recoverable runtime errrors.

```c
struct GError {
  GQuark       domain;  // the module the error-reporting function is located in
  gint         code;    // the specific error that occurred
  gchar       *message; // user-readable error message 
};
```

Precondition checks should be done by `g_return_if_*` macros.
