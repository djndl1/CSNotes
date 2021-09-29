# Type System

Relies on the threading API, the hashtable 

## `GType`

The `GType` API is the fondation of the GObject system, which provides the facilities for registering and managing all fundamental data types, user-defined object and interface types. Every type has an integer type id of `GType`, which uniquely identifies a type.

Types are divided into _fundamental types_ (top-level types which do not derive from any other type) and _derived types_, with `G_TYPE_FUNDMANETAL_MAX` as the boundary type id.

```c
struct _GTypeClass
{
  GType g_type;
};
struct _GTypeInstance
{
  GTypeClass *g_class;
};
```

The type structure has the type's type id and the instance structure maintains its type info by holding a pointer to its type structure (in C++ jargon, the vtable). Note that `GTypeClass` is not the internal structure of type metadata, not even `GTypeInfo` or `GTypePlugin`, which is merely a function parameter type for the registration of a type.

### Type Maintenance

Basic type metadata is bookkept in a `TypeNode` structure. The type id of a derived static type is the address of its `TypeNode`. This address (type id) is ensured to be aligned at least on a 4-byte boundary due to the alignment requirement of `malloc`, 
hence `g_assert ((type & TYPE_ID_MASK) == 0)` should never fail. The fundamental types would have its `GTypeFundamentalFlags` prepend to this node structure. The size of the `supers` array is dependent on the type's parents. The memory of these three parts are allocated together.

```c
struct _GTypeFundamentalInfo
{
  GTypeFundamentalFlags  type_flags;
};

struct _TypeNode
{
  guint volatile ref_count;
#ifdef G_ENABLE_DEBUG
  guint volatile instance_count;
#endif
  GTypePlugin *plugin;
  guint        n_children; /* writable with lock */
  guint        n_supers : 8;
  guint        n_prerequisites : 9;
  guint        is_classed : 1;
  guint        is_instantiatable : 1;
  guint        mutatable_check_cache : 1;	/* combines some common path checks */
  GType       *children; /* writable with lock */
  TypeData * volatile data;
  GQuark       qname; /* the quark of the type name */
  GData       *global_gdata;
  union {
    GAtomicArray iface_entries;		/* for !iface types */
    GAtomicArray offsets;
  } _prot;
  GType       *prerequisites;
  GType        supers[1]; /* flexible array, self + ancestors + NULL */
};

/**
* A variant type storing type data
*/
union _TypeData
{
  CommonData         common;
  BoxedData          boxed;
  IFaceData          iface;
  ClassData          class;
  InstanceData       instance;
};
```

Static type ids are maintained in a hash table `static_type_nodes_ht`(`<gchar*, GType`>). In addition, fundamental types are also maintained in the `static_fundamental_type_nodes` array , both protected by a read-write lock. 

### Type Registration

1. Type names are required to be longer than three characters, and starts with the ten digits and the twenty-six letters with `-`, `_` and `+` added to the characters available following (hardcoded into the type system of GLib) and not synonymous with those of existing types.

```c
/**
* Get the type data node by type id
*/
static inline TypeNode*
lookup_type_node_I (GType utype);

/**
* check if the given type name is valid and already used 
*/
static gboolean
check_type_name_I (const gchar *type_name)
```



2. The given type id is checked to see 
  + for a fundamental type, if it uses a valid fundamental id, already exists as a fundamental type, is instantiable but not classed (instantiable implies classed)
  + For a static type, if it exists, is flatly and deeply derivable (multi-level hierarchy). 

```c
/**
* Check if the given type is existent, flatly derivable and deeply derivable
*/
static gboolean
check_derivation_I (GType        parent_type,
		    const gchar *type_name);

/**
* get type name by type id
*/
type_descriptive_name_I (GType type);

/**
* Get the fundamental type of a give type id (may or may not be a fundamental type)
*/
static inline GTypeFundamentalInfo*
type_node_fundamental_info_I (TypeNode *node)
```

```c
/**
* increment the refcount of the given node and its parent type node
*/
static inline void
type_data_ref_Wm (TypeNode *node);
```

```c
/**
* Make a new fundamental type node
*/
static TypeNode*
type_node_fundamental_new_W (GType                 ftype,
			     const gchar          *name,
			     GTypeFundamentalFlags type_flags);
         

/**
* Creates a type node whether fundamental, static or dynamic. 
* Used as the helper function to construct a certain category of type nodes
*/
static TypeNode*
type_node_any_new_W (TypeNode             *pnode,
		     GType                 ftype,
		     const gchar          *name,
		     GTypePlugin          *plugin,
		     GTypeFundamentalFlags type_flags);
         
/**
* various checks on the type flags and state of a type
*/
static gboolean
check_type_info_I (TypeNode        *pnode,
		   GType            ftype,
		   const gchar     *type_name,
		   const GTypeInfo *info);
       
/**
 * create and populate the TypeData variant
 */
static void
type_data_make_W (TypeNode              *node,
		  const GTypeInfo       *info,
		  const GTypeValueTable *value_table)
```

## Use `GObject`

The GObject library contains an implementation for a base fundamental type named `GObject` with

1. refcount memory management

2. ctor/dtor of instances

3. generic per-object properties with setter/getter

4. easy use of signals


# Every Basic Type

## GQuark

Use: when both integers and string can be used interchangeably, or used for string interning.

The string-to-int mapping is maintained as a hash table `quark_ht`. The reverse is a one-based (since quarks are non-zero, 0 denotes `NULL`) string array `quarks` (`char **` of course, allocated per `QUARK_BLOCK_SIZE`), the `quark_seq_id`  is the index for the next new quark. A mutex is used for thread-safe access to the quark structures. These structures are initialized at load time of glib. Note the old quark string array is leaked every time the new one replaces the old one.

Multiple duplicated strings are stored in string blocks if small enough, rather than scattered around.

# Concurrency

Independent of the GObject type system, using only some typedefs of `gtypes.h`.

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
