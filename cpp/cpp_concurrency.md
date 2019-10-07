In C++ the multi-threading implementation offered by the language is a high level interfeace to multithreading, using the raw pthread building blocks is hardly ever necessary.

A _thread of execution_ is a single flow of control within a program. All threads share the process's data and code. Access must be synchronized to avoid that threads read data while these data are being modified by other threads and to avoid that multiple threads modify the same data at the same time.

# Handling time

`sleep` and `select` are designed at a time when multithreading was not commonly available. The STL offers dedicated classes for specifying time.

When specifying time, an appropriate time unit must be selected. Factors that can be used in combination with time units are defined in the class template `std::ratio`. The time units themselves are defined in the namespace `chrono`.

Objects of the class `std::chrono::duration` define amounts of time. It requires a numeric type defining the druation's value and a time-unit called its _period_ (where `ratio` comes in). Various predefined duration types exist.

C++ offers several predefined `clock` types:

- `std::chrono::system_clock`: the system's real time clock, the wall clock

- `std::chrono::steady_clock`: a clock whose time increases in parallel with the increase of real time

- `std::chrono::high_resolution_clock`: in practice, the same clock as `system_clock`.

Objects of the class `std::chrono::time_point` define a point in time.

`std::put_time` formats time.

# Multi Threading

Higher levels of abstraction are usually semantically closer to the original problem description, resulting in code which is easier to understand and therefore easier to maintain. Also, high-abstraction classes also provide exception safety and prevent the occurrence of memory leak.

The main tool is `std::thread`; characteristics of individual threads can be queried from the `std::this_thread` namespace and it also offers some control over the behavior of an individual thread.

`std::mutex` and `std::condition_variable` are offered to synchronize access to shared data.

Members of these classes may throw `system_errors` objects when encountering a low-level error condition.

## `std::this_thread`

Several free functions are provided, providing information about the current thread or that can be used to control its behavior.

- `thread::id this_thread::get_id() noexcept`: return an object that identifies the currently active thread of execution.

- `void yield() noexcept`: the current thread is briefly suspended, allowing other (waiting) threads to start.

- `void sleep_for(chrono::duration<Rep, Period> const& relTime) noexcept`: it is suspended for the time specified

- `void sleep_until(chrono::time_point<Clock, Duration> const &absTime) noexcept`: suspended until the specified `absTime` is in the past.

## `std::thread`

- `explicit thread(Fun &&fun, Args &&... args)`: this member _template_ expects a function or functor as its first argument. The function is started immediately as a separate thread. The types of the arguments that are passed to the `thread` constructor and that are expected by the called function must match. 

Be careful when passing local variables as arguments to thread objects. If the thread continues to run when the function whose local variables are used terminates, then the thread suddenly uses wild pointers or wild references, as the local variables no longer exist. Pass an anonymous copy of the local variable as argument to the thread constructor or call `join` on the thread object to ensure the thread has finished within the local variable lifetime.

```cpp
void threadFun(std::string const& text)
{
    for (size_t iter = 1; iter != 6; ++iter) {
        std::cout << text << '\n';
        std::this_thread::sleep_for(std::chrono::seconds{1});
    }
}

// a thread factory function
std::thread safeLocal()
{
    std::string text = "hello world";
    return std::thread(threadFun, std::string(text)); // ensure that `text` is not passed by reference
}
```

The class `std::thread` does not provide a copy constructor.

- `void detach()`: The thread for which `detach` is called continues to run. The parent thread calling `detach` continues immediately beyond the `detach` call. The `object` as in `object.detach()` no longer represents a thread of execution. As the `main` thread exits, the still running detached threads also stop. A detached thread may very well continue to run after the function that launched it has finished.

```cpp
void fun(size_t count, char const* text)
{
    for (; count--; ) {
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
        std::cout << count << ": " << text << std::endl;
    }
}

int main(int argc, char *argv[])
{
    std::thread first{fun, 5, "hello world"};
    first.detach();

    std::thread second(fun, 5, "a second thread");
    second.detach();

    std::this_thread::sleep_for(std::chrono::milliseconds(400));
    std::cout << "leaving from the main thread" << std::endl;
    return 0;
}
```

```bash
djn@djn-Aspire-E5-575G:~/FOSS/playground $ ./a.out
44: : a second threadhello world

33: hello world: a second thread

22: hello world: a second thread
```

```cpp
void add(int const& p1, int const &p2)
{
    this_thread::sleep_for(milliseconds(200));
    cerr << p1 << " + " << p2 << " = " << (p1 + p2) << '\n';
}

void run()
{
    int v1 = 10;
    int v2 = 10;

    thread{add, ref(v1), ref(v2)}.detach();
}
// output:32765 + 924796728 = 924829493
```

- `id get_id()` returns the thread's unique ID if running, otherwise returns `thread::id()`. 

```cpp
// libcxx simply returns the underlying thread identifier and cast it to `thread::id`
class _LIBCPP_TYPE_VIS thread
{
    __libcpp_thread_t __t_;

    thread(const thread&);
    thread& operator=(const thread&);
public:
    typedef __thread_id id;
    typedef __libcpp_thread_t native_handle_type;
    //...
    id get_id() const _NOEXCEPT {return __libcpp_thread_get_id(&__t_);}
};

__libcpp_thread_id __libcpp_thread_get_id(const __libcpp_thread_t *__t)
{
  return *__t;
}
// more interestingly, `__thread_id` is just a wrapper around `__libcpp_thread_t` and has only this one data member.
```

- `bool joinable()`: returns `get_id() != id()`

```cpp
// in libcxx, id() defaults to 0, after detaching, __t_ is set to zero
bool joinable() const _NOEXCEPT {return !__libcpp_thread_isnull(&__t_);}

bool __libcpp_thread_isnull(const __libcpp_thread_t *__t) {
  return *__t == 0;
}
```

Exceptions thrown from the thread are local to the executed thread. Either they are caught on spot or are passed to the starting thread.

When a `thread` object is destroyed while its thread function is still running, `terminate` is called, aborting the program's end. 

```cpp
thread::~thread()
{
    if (!__libcpp_thread_isnull(&__t_)) 
    // That's why the starting thread either joins or detaches the spawned thread, both of which set `__t_` to zero..
        terminate();
}
```

The `thread_local` keyword provides an intermediate data level that is uniquely available to different threads. Global variables declared as `thread_local` are global within each individual thread. `thread_local` is implicitly `static` otherwise it makes no sense to declare a local `thread_local`.

To avoid the following code:

```cpp
void parent() 
{
    thread child{childAction};
    try {
        doSomeWork(); // may throw exceptions
        child.join();
    } catch (...) {
        child.join();
        throw;
    }
}
```

we may consider using RAII, that is, automatically join the thread when going out of scope

```cpp
class JoinGuard {
    std::thread d_thread;

public:
    JoinGuard(std::thread &&threadObj) : d_thread(std::move(threadObj))
        {}

    ~JoinGuard()
        {
            if (d_thread.joinable())
                d_thread.join();
        }
};
```

```cpp
void parent()
{
    JoinGuard{std::thread{childAction}};
    doSomeWork();
}

int main(int argc, char *argv[])
{
    try {
            parent();
  // may throw exception, but JoinGuard ensures the spawned thread is always joined
    } catch (...) {
        // ..
    }
    return 0;
}
```

# Synchronization

Objects of mutex classes are used to protect shared data. The members of mutex classes perform atomic actions: no context switch occurs while they are active. Although mutexes can directly be used in programs, this rarely happened. It is more common to embed mutex handling in locking classes that make sure that the mutex is automatically unlocked again when the mutex lock is no longer needed.

Classes `std::timed_mutex` and `std::timed_recurisve_mutex` are available for delayed lock release.

The destructor of class `mutex` does not unlock a mutex. A `system_error` is thrown when relocking a non-recursive mutex or unlocking a lock not owned by the thread. `try_lock()` tries to obtain ownership of the mutex and returns `false` if failed. Timed mutexes can also try to obtain ownership of the mutex within the specified time interval and can keep the mutex until a specified time point has passed.

When using a singleton concurrently, multiple instances may be constructed by different threads at the same time. Mutexes may be used but it's expensive. Another method is to use `std::call_once`.

```cpp
class Singleton {
    static std::once_flag s_once;
    static Singleton *s_singleton;
    ...

    public:
    static Singleton *instance()
        {
            std::call_once(s_once, []{s_singleton = new Singleton;});
            return s_singleton;
            
        }
};
```

A `constexpr`, if statisfying the requirements for constant initialization, is guaranteed to be initialized before any code is run as part of the static initialization. A static variable defined within a compound statement are initialized the first time the function is called at the point in the code where the static variable is defined. This feature causes a thread to wait automatically if another thread is still initializing the static data (by C++ standard). In both cases, concurrent initialization shouldn't be a problem.


(C++17) The class `shared_mutex` provides a non-recursive mutex with shared ownership semantics. Multiple threads can simultaneously hold a shared lock ownership of a `shared_mutex` type of object. But no thread can hold a shared lock while another thread holds an exclusive lock on the same `shared_mutex` object and vice versa.

## Lock and Lock Handling

Locks are used to simplify the use of mutexes. To simplify locking and unlocking, a few mutex wrappers are available:

- `std::lock_guard`: basic unlock_guarantee, a basic wrapper. This wrapper can accept a mutex that the thread has already acquired.

```cpp
template <class _Mutex>
class _LIBCPP_TEMPLATE_VIS _LIBCPP_THREAD_SAFETY_ANNOTATION(scoped_lockable)
lock_guard
{
public:
    typedef _Mutex mutex_type;

private:
    mutex_type& __m_;
public:

    _LIBCPP_NODISCARD_EXT _LIBCPP_INLINE_VISIBILITY
    explicit lock_guard(mutex_type& __m) _LIBCPP_THREAD_SAFETY_ANNOTATION(acquire_capability(__m))
        : __m_(__m) {__m_.lock();}

    _LIBCPP_NODISCARD_EXT _LIBCPP_INLINE_VISIBILITY
    lock_guard(mutex_type& __m, adopt_lock_t) _LIBCPP_THREAD_SAFETY_ANNOTATION(requires_capability(__m))
        : __m_(__m) {}
    _LIBCPP_INLINE_VISIBILITY
    ~lock_guard() _LIBCPP_THREAD_SAFETY_ANNOTATION(release_capability()) {__m_.unlock();}

private:
    lock_guard(lock_guard const&) _LIBCPP_EQUAL_DELETE;
    lock_guard& operator=(lock_guard const&) _LIBCPP_EQUAL_DELETE;
};
```

- `std::unique_lock`: a more expensive interface, allowing explicit unlocking and locking of the mutex they control. a general-purpose mutex ownership wrapper allowing deferred locking, time-constrained attempts at locking, recursive locking, transfer of lock ownership, and use with condition variables. It can be constructed without locking the mutex and can `release` the mutex and no longer "own"s it (can no longer manipulate it, break the association with the mutex) without unlocking the mutex.

- `std::shared_lock`: a general-purpose shared mutex ownership wrapper allowing deferred locking, timed locking and transfer of lock ownership. Used with `shared_mutex` and `shared_timedmutex`


## Deadlocks

If multiple mutexes must be used, always obtain the locks in the same order. C++ defines the generic `std::lock` and `std::try_lock` functions that can be used to help prevent deadlocks rather than do it manually.

## Conditional Variables (Event Handling)

Condition variables allow programs to synchronize threads using the states of data, rather than simply locking the access to data. If using mutexes only, the above consumer-producer scenario would lack information about the states of data, that is, consumers don't know if they should acquire the mutex and so they are busy acquiring and releasing the mutex just to check if the the data is available. With condition variables, mutexes are only used for controlling access to the data. In addition, condition variables allow threads to release ownership of mutexes until a certain value has been obtained, until a preset amount of time has been passed, or until a preset point in time has been reached.

The prototypical setup of threads using condition variables:

1. consumer:

- lock the mutex

- while the required condition has not yet been attained (i.e., is false): release the mutex and wait until being notified

- once the mutex's lock has been reacquired, and the required condition has been attained: process the data

- release the mutex's lock

2. producer:

- lock the mutex

- while the required condition has not yet been attained, do something to attain the required condition

- notify waiting threads

- release the mutex's lock.

The consumer will miss the producer's notification if it hasn't yet entered its waiting state. So waiting (consumer) threads should start before notifying (producer) threads.

`std::condition_variable`s are used in combination with `unique_lock<mutex>`, which might be more efficient than the more general `std::condition_variable_any` that may be used with any lock type.

In addition to the condition variables, `std::notify_all_at_thread_exit` notifies all threads blocking on the condition variable. Waiting threads must verify the thread that is waiter for has indeed exited by first obtaining the lock and verify if the condition they are waiting for has been satisfied.

`condition_variable`'s destructor requires that no thread is blocked by the thread destroying the `condition_variable`. All threads waiting on a `condition_variable` must be notified before a `condition_variable` object's lifetime ends.

The functionality that is offered by `condition_variable_any` is identical to the functionality offered by the class `condition_variable`, albeit that the lock type is not predefined. The requirements of these lock-types are identical to those of the stl-provided `unique-lock` and user-defined lock-type implementation should provide at least the interface and semantics that is also provided by `unique_lock`.

```cpp
#include <mutex>
#include <condition_variable>

class Semaphore
{
    mutable std::mutex d_mutex;
    std::condition_variable d_condition;
    size_t d_nAvailable;

    public:
        Semaphore(size_t nAvailable);

        void wait();      // wait # available
        void notify_all();    // notify_all # available, notify if initially 0

        size_t size() const;
};

Semaphore::Semaphore(size_t nAvailable)
:
    d_nAvailable(nAvailable)
{}

size_t  Semaphore::size() const
{
    lock_guard<mutex> lk(d_mutex);    // get the lock
    return d_nAvailable;
}

void Semaphore::notify_all()
{
    lock_guard<mutex> lk(d_mutex);    // get the lock
    if (d_nAvailable++ == 0)
        d_condition.notify_all();   // use notify_one to notify one other
                                    // thread
}

void Semaphore::wait()
{
    unique_lock<mutex> lk(d_mutex);   // get the lock
    while (d_nAvailable == 0)
        d_condition.wait(lk);   // internally releases the lock
                                // and waits, on exit
                                // acquires the lock again
    --d_nAvailable;              // dec. semaphore
}

```

## Atomic Actions

Atomic data types are available for all basic types and also for trivial user defined types, which are all scalar types, arrays of elements of a trivial type, and classes whose constructors, copy constructors, and destructors all have default implementations and their non-static data members are themselves of trivial types.

The class template `std::atomic<T>` is available for all built-in types, including pointer types. `std::atomic<Trivial>` also defines an atomic variant of a trivial type.

Atomic types cannot be assigned to each other directly, but can be assgined to or initialized using non-atomic types.

```cpp
atomic<int> a1 = 5;
atomic<int> a2{static_cast<int>(a1)};
```

`std::memory_order`: http://senlinzhan.github.io/2017/12/04/cpp-memory-order

https://www.zhihu.com/question/24301047



There are some standard available member functions for `std::atomic<T>`:

First read [CAS](https://en.wikipedia.org/wiki/Compare-and-swap), [cmpxchg](http://heather.cs.ucdavis.edu/~matloff/50/PLN/lock.pdf)

- `bool compare_exchange_strong()`

- `bool compare_exchange_weak()`

- `T exchange(T newValue)`

- `bool is_lock_free()`: if operations on the current object can be performed lock-free

- `void store(T newValue)`: same as assignment

- `T load() const`

Integral atomic types `Integral` also offers:

- `fetch_add`; `fetch_sub`; `fetch_and`; `fetch_!=``; `fetch_^|`, increments, decrements, assignments.

`std::atomic_compare_exchange_strong`; `std::atomic_compare_exchange_weak`; `std::atomic_exchange`; `std::atomic_init` (unfortunately it's not atomic); `std::atomic_is_lock_free`; `std::atomic_load` are available for all atomic types.

## Shared States

In multithreaded programs several classes and functions can be used that produce shared states, making it easy to communicate results to other threads. Objects that contain such shared states are called _asynchronous return objects_. 

A thread may request the results of an asynchronous return object before these results are actually available. Asynchronous return objects offer `wait` and `get` members which, respectively, wait until the results have become available, and produce the asynchronous results once they are available. 

Shared states are made ready by _asynchronous providers_. Asynchronous providers are simply objects or functions providing results to shared states. An asynchronous provider marks its shared states as being ready and unblocks any waiting threads. Shared states use reference counting to keep track of the number of asynchronous return objects or asynchronous providers that hold references to them. 

Objects of the class `std::future` are asynchronous return objects, produced by `std::async`, `std::packaged_task` and `std::promise`.

### `std::future` in `<future>`

Rather than waiting and using locks it would be nice if some asynchronous task could be started, allowing the initiating thread (or even other threads) to pick up the result at some point in the future, when the results are needed, without having to worry about data locks or waiting times. 

Objects of the class template `std::future` harbor the results produced by asynchronously executed tasks. The asynchronously executed task may throw an exception (ending the task). In that case the future object catches the exception, and rethrows it once its return value (i.e., the value returned by the asynchronously executed task) is requested. Error conditions are returned through `std::future_error` exceptions.

### `std::async` (`<future>`) starts a new thread

`async` is used to start asynchronous tasks, returning values (or `void`) to the calling thread.

If an exception leaves a thread, then the program ends. This scenario doesn’t occur when `std::async` is used. `Async` may start a new asynchronous task, and the activating thread may retrieve the return value of the function implementing the asynchronous task or any exception leaving that function from a `std::future` object returned by the `async` function.

`std::async` accepts the `std::launch` enum to determine which launch policy to follow.

```cpp
#include <iostream>
#include <future>

int fun()
{
    std::cerr << "    hello from fun\n";
    return 12;
}

int indirect(std::future<int> &fut)
{
    std::cerr << "calling indirect to obtain fun's results\n";
    return fut.get();
}

int main()
{
    auto fut = std::async(fun);

    auto ret = std::async(std::launch::async, indirect, std::ref(fut));
    std::cerr << "receiving from fun: " << ret.get() << '\n';
}
```

```bash
receiving from fun: calling indirect to obtain fun's results
    hello from fun
12
```

### `std::promise` in `<future>`

A `promise` is useful to obtain the results from another thread without further synchronization requirements.

```cpp
#include <iostream>
#include <future>

using namespace std;

void compute1(promise<int> &ref)
{
    ref.set_value(9);
}

int main()
{
    std::promise<int> p;
    std::thread(compute, ref(p)).detach();

    cout << p.get_future().get() << '\n'; // the main thread blocks here
}
```

Promises can be useful when implementing a multithreaded version of some algorithm without having to use additional synchronization statements.

```cpp
#include <iostream>
#include <future>
#include <iomanip>

using namespace std;

//code
int m1[2][2] = {{1, 2}, 
                {3, 4}};
int m2[2][2] = {{3, 4}, 
                {5, 6}};

void innerProduct(promise<int> &ref, int row, int col)
{
    int sum = 0;
    for (int idx = 0; idx != 2; ++idx)
        sum += m1[row][idx] * m2[idx][col];

    ref.set_value(sum);
}

int main()
{
    promise<int> result[2][2];

    for (int row = 0; row != 2; ++row)
    {
        for (int col = 0; col != 2; ++col)
            thread(innerProduct, ref(result[row][col]), row, col).detach();
    }

    for (int row = 0; row != 2; ++row)
    {
        for (int col = 0; col != 2; ++col)
            cout << setw(3) << result[row][col].get_future().get();
        cout << '\n';
    }
}
```

```bash
 13 16
 29 36
```

## `std::packaged_task` in `<future>`

`std::packaged_task` allows a program to package a function or functor and pass the package to a thread for further processing.

Calling a `packaged_task`, is not identical to calling the harbored function, but eventually its function will be called.

```cpp
#include <iostream>
#include <fstream>
#include <future>
#include <mutex>
#include <thread>
#include <condition_variable>

using namespace std;

//code
mutex carDetailsMutex;
condition_variable condition;
string carDetails;
packaged_task<size_t (std::string const &)> serviceTask;

size_t volkswagen(string const &type)
{
    cout << "performing maintenance by the book for a " << type << '\n';
    return type.size() * 75;            // the size of the bill
}

size_t peugeot(string const &type)
{
    cout << "performing quick and dirty maintenance for a " << type << '\n';
    return type.size() * 50;             // the size of the bill
}

void garage()
{
    while (true)
    {
        unique_lock<mutex> lk(carDetailsMutex);
        while (carDetails.empty()) // the condition may be satisfied from the very start, then it just goes on
            condition.wait(lk);

        cout << "servicing a " << carDetails << '\n';
        serviceTask(carDetails);
        carDetails.clear();
    }
}

int main()
{
    thread(garage).detach();

    while (true)
    {
        string car;
        if (not getline(cin, car) || car.empty())
            break;
        {
            lock_guard<mutex> lk(carDetailsMutex);
            carDetails = car;
            serviceTask =  packaged_task<size_t (string const &)>(
                    car[0] == 'v' ? volkswagen : peugeot
                );
            condition.notify_one();
        }
        
        auto bill = serviceTask.get_future();
        cout << "Bill for servicing a " << car <<
                                ": EUR " << bill.get() << '\n';
    }
}
```

https://stackoverflow.com/questions/18143661/what-is-the-difference-between-packaged-task-and-async
