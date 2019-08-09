# Concept

All threads within a single process have access to the same process components, such as file descriptors and memory. We can simplify code that deals with asynchronous events by assigning a separate thread to handle each event type. The overall throughput may be improved and interactive programs can realize improved response time by using multiple threads to separpate to the portions of the program that deal with user input and output from the other parts of the program.

A thread consists of the information necessary to represent an execution context within a process, including a thread ID, a set of registers values, a stack, a scheduling priority and policy, a signal mask, an `errno` variable and thread-specific ddata. Everything with a process is sharable among the threads in a process, including the text of executable program, the program's global and heap memory, the stacks, and the file descriptors.

# Thread Identification

A thread ID is represented by the `pthread_t` data type, which may not be an integer (it is a `struct` on linux) and provides no portable way to print its value for debugging. `pthread_equal()` compares two thread IDs and `pthread_self()` obtains the current thread ID. Thread IDs may be used to assign a job to a particular thread.

# Thread Creation

When a program runs, it starts out as a single process with a single thread of control until it creates  more threads of control by `pthread_create()`. If more than one argument to the function is needed, store them in a struct and pass the pointer. pthread functions usually return an error code when they fail. They don't set `errno` like other POSIX functions. The new thread cannot safely use the `thread_t` that handles it because the spawning thread may not have returned from `pthread_create()` thus the `thread_t` may not usable.

