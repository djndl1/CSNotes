In C++ the multi-threading implementation offered by the language is a high level interfeace to multithreading, using the raw pthread building blocks is hardly ever necessary.

A _thread of execution_ is a single flow of control within a program. All threads share the process's data and code. Access must be synchronized to avoid that threads read data while these data are being modified by other threads and to avoid that multiple threads modify the same data at the same time.

# Handling time

TODO

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

- `void detach()`: The thread for which `detach` is called continues to run. The parent thread calling `detach` continues immediately beyond the `detach` call. The `object` as in `object.detach()` no longer represents a thread of execution. As the `main` thread exits, the still running detached threads also stop. A detached thread may bery well continue to run after the function that launched it has finished.

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
