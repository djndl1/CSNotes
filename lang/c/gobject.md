# Every Basic Type

## GQuark

Use: when both integers and string can be used interchangeably, or used for string interning.

The string-to-int mapping is maintained as a hash table `quark_ht`. The reverse is a one-based (since quarks are non-zero, 0 denotes `NULL`) string array `quarks` (`char **` of course, allocated per `QUARK_BLOCK_SIZE`), the `quark_seq_id`  is the index for the next new quark. A mutex is used for thread-safe access to the quark structures. These structures are initialized at load time of glib. Note the old quark string array is leaked every time the new one replaces the old one.

Multiple duplicated strings are stored in string blocks if small enough, rather than scattered around.

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

# `GError`

Used to report recoverable runtime errrors.

```c
struct GError {
  GQuark       domain;  // the module the error-reporting function is located in
  gint         code;    // the specific error that occurred
  gchar       *message; // user-readable error message 
};
```

Precondition checks should be done by `g_return_if_*` macros.
